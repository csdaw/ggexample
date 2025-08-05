#' Geom Spring
#'
#' @description This will be the description.
#'
#' @inheritParams ggplot2::geom_path
#' @param n `integer`, number of points used per revolution. Defines the
#' visual fidelity of the spring.
#'
#' @returns This will return something.
#' @export
#'
#' @examples
#' # This will be an example
#'
geom_spring <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = "identity",
                        ...,
                        n = 50,
                        arrow = NULL,
                        lineend = "butt",
                        linejoin = "round",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSpring,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

stat_spring <- function(mapping = NULL,
                        data = NULL,
                        geom = "path",
                        position = "identity",
                        ...,
                        n = 50,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSpring,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}

StatSpring <- ggproto("StatSpring", Stat,

                      # Fields -------------------------------------------------
                      required_aes = c("x", "y", "xend", "yend"),
                      # non_missing_aes = character(),
                      optional_aes = c("diameter", "tension"),
                      # default_aes = aes(),
                      # dropped_aes = character(),
                      # extra_params = "na.rm",
                      # retransform = TRUE,

                      # Methods ------------------------------------------------

                      ## compute_statistic -------------------------------------

                      # setup_params = function(data, params)
                      # Output: `params`


                      setup_data = function(data, params) {
                        if (anyDuplicated(data$group)) {
                          data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
                        }
                        data
                      },

                      # compute_layer = function(self, data, params, layout)
                      # Input: `data` has 5 rows for 5 springs, columns = tension, diameter, x, y, xend, yend, PANEL, group
                      # Output: `data` which now has 8993 rpws with the full path x/y coordinates of the 5 springs
                      # This is performed per PANEL.

                      compute_panel = function(data, scales, n = 50) {
                        cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
                        springs <- lapply(seq_len(nrow(data)), function(i) {
                          spring_path <- create_spring(
                            data$x[i],
                            data$y[i],
                            data$xend[i],
                            data$yend[i],
                            data$diameter[i],
                            data$tension[i],
                            n
                          )
                          cbind(spring_path, unclass(data[i, cols_to_keep]))
                        })
                        do.call(rbind, springs)
                      },

                      # compute_group = function(self, data, scales)
                      # This function is defined in Stat, but apparently 'not implemented'

                      ## finish_statistics -------------------------------------

                      # finish_layer = function(self, data, params)
                      # Output: `data`

                      ## utilities ---------------------------------------------

                      # parameters = function(self, extra = FALSE)
                      # This kind of loops through input arguments, and sees
                      # whether they're used as a grouping or not?? Maybe?
                      # Output: vector of args e.g. c("n", "na.rm")? Or just "n"?

                      # aesthetics = function(self)
                      # Output: vector of aesthetic column names, and "group"


)

GeomSpring <- ggproto("GeomSpring", Geom,

                      # Fields -------------------------------------------------

                      required_aes = c("x", "y", "xend", "yend"),
                      # non_missing_aes = character(),
                      # optional_aes = character(),
                      default_aes = aes(
                        colour = "black",
                        linewidth = 0.5,
                        linetype = 1L,
                        alpha = NA,
                        diameter = 1,
                        tension = 0.75
                      ),
                      # rename_size = FALSE,
                      # extra_params = c("na.rm")
                      # draw_key = ggplot2::draw_key_point,

                      # Methods ------------------------------------------------

                      ## compute_geom_1 ----------------------------------------

                      # setup_params = function(data, params)
                      # Output: `params`

                      # Ensure that each row has a unique group id
                      setup_data = function(data, params) {
                        if (is.null(data$group)) {
                          data$group <- seq_len(nrow(data))
                        }
                        if (anyDuplicated(data$group)) {
                          data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
                        }
                        data
                      },

                      ## compute_geom_2 ----------------------------------------

                      # use_defaults = function(self, data, params = list(),
                      #                         modifiers = aes(), default_aes = NULL, theme = NULL, ...)
                      # Output: `data`
                      # "A function method for completing the layer data by
                      # filling in default aesthetics that are not present.
                      # It is not recommended to use as an extension point."

                      ## draw_geom ---------------------------------------------

                      # handle_na = function(self, data, params)
                      # Output: `data` I think?
                      # "A function method to handle missing values. The default method will
                      # remove rows that have missing values for the aesthetics listed in the
                      # `required_aes` and `non_missing_aes` fields. It is not recommended to
                      # use this method as an extension point."

                      # draw_layer = function(self, data, params, layout, coord)
                      # Output: Not sure, `data`?
                      # "A function method orchestrating the drawing of the entire layer. The
                      # default method splits the data and passes on drawing tasks to the
                      # panel-level `draw_panel()` method. It is not recommended to use this method
                      # as an extension point."

                      # Transform/split the data into groups.
                      # "The default `draw_panel()` method splits the data into groups,
                      # passes on the drawing tasks to the group-level `draw_group()` method and
                      # finally assembles these into a single grob. The default `draw_group` method
                      # is not implemented."
                      draw_panel = function(data,
                                            panel_params,
                                            coord,
                                            n = 50,
                                            arrow = NULL,
                                            lineend = "butt",
                                            linejoin = "round",
                                            linemitre = 10,
                                            na.rm = FALSE) {

                        # CSD NOTE: you can get the axis limits of the PANEL itself
                        # (not just the data) by the following:
                        panel_params$x.range
                        panel_params$y.range

                        # Transform the input data to specify the spring paths
                        cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
                        springs <- lapply(seq_len(nrow(data)), function(i) {
                          spring_path <- create_spring(
                            data$x[i],
                            data$y[i],
                            data$xend[i],
                            data$yend[i],
                            data$diameter[i],
                            data$tension[i],
                            n
                          )
                          cbind(spring_path, unclass(data[i, cols_to_keep]))
                        })
                        springs <- do.call(rbind, springs)

                        # Use the draw_panel() method from GeomPath to do the drawing
                        GeomPath$draw_panel(
                          data = springs,
                          panel_params = panel_params,
                          coord = coord,
                          arrow = arrow,
                          lineend = lineend,
                          linejoin = linejoin,
                          linemitre = linemitre,
                          na.rm = na.rm
                        )
                      }

                      ## utilities ---------------------------------------------

                      # parameters = function(self, extra = FALSE)
                      # This kind of loops through input arguments, and sees
                      # whether they're used as a grouping or not?? Maybe?
                      # Output: vector of args e.g. c("n", "na.rm")? Or just "n"?

                      # aesthetics = function(self)
                      # Output: vector of aesthetic column names, and "group"
)
