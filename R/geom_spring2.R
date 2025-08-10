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
geom_spring2 <- function(mapping = NULL,
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
    geom = GeomSpring2,
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

GeomSpring2 <- ggproto("GeomSpring2", Geom,

                       # Fields ------------------------------------------------

                       # Specify the default and required aesthetics
                       required_aes = c("x", "y", "xend", "yend"),
                       # non_missing_aes = character(),
                       # optional_aes = character(),
                       default_aes = aes(
                         colour = "black",
                         linewidth = 0.5,
                         linetype = 1L,
                         alpha = NA,
                         diameter = 0.35,
                         tension = 0.75
                       ),
                       # rename_size = FALSE,
                       # extra_params = c("na.rm")
                       draw_key = draw_key_spring,

                       # Methods ------------------------------------------------

                       ## compute_geom_1 ---------------------------------------

                       # Check that the user has specified sensible parameters
                       setup_params = function(data, params) {
                         if (is.null(params$n)) {
                           params$n <- 50
                         } else if (params$n <= 0) {
                           rlang::abort("Springs must be defined with `n` greater than 0")
                         }
                         params # output is a named list
                       },

                       # setup_data = function(data, params) {
                       #   data
                       # },

                       ## compute_geom_2 ---------------------------------------

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
                       # Input:
                       #   `data` = data.frame with columns: tension, diameter, x, y, xend, yend, PANEL, group, colour, linewidth, linetype, alpha
                       #   `panel_params` = a list of things, mostly ggproto objects, but also a couple of vectors
                       #   `coord` = a ggproto object
                       #   The other inputs are self explanatory.
                       #
                       # "The default `draw_panel()` method splits the data into groups,
                       # passes on the drawing tasks to the group-level `draw_group()` method and
                       # finally assembles these into a single grob. The default `draw_group` method
                       # is not implemented."
                       #
                       # To correctly scale the x/y data, one should always call
                       # `coord$transform(data, panel_params)`. When working with non-linear
                       #' coordinate systems, data should be converted to fit a primitive geom
                       #' (e.g. point, path or polygon) and passed on to the corresponding draw
                       #' method for [munching][coord_munch()].
                       #
                       # Output: springGrob()
                       draw_panel = function(data,
                                             panel_params,
                                             coord,
                                             n = 50,
                                             lineend = "butt",
                                             na.rm = FALSE) {

                         # Remove missing data, returning early if all are missing
                         data <- ggplot2::remove_missing(
                           df = data,
                           na.rm = na.rm,
                           vars = c("x", "y", "xend", "yend", "linetype", "linewidth"),
                           name = "geom_spring"
                         )
                         if (is.null(data) || nrow(data) == 0) return(ggplot2::zeroGrob())

                         # Supply the coordinate system for the plot
                         if (!coord$is_linear()) {
                           rlang::warn(
                             "spring geom only works correctly on linear coordinate systems"
                           )
                         }

                         # CRITICAL STEP! Turns x/y/xend/yend values into npc (between 0 and 1)
                         # Output: data.frame same as `data`, but the columns x, y, xend, yend
                         # are scaled between 0 and 1.
                         coord <- coord$transform(data, panel_params)

                         # Construct the grob
                         springGrob(
                           coord$x,
                           coord$y,
                           coord$xend,
                           coord$yend,
                           default.units = "native",
                           diameter = unit(coord$diameter, "cm"),
                           tension = coord$tension,
                           n = n,
                           gp = gpar(
                             col = alpha(coord$colour, coord$alpha),
                             lwd = coord$linewidth * .pt,
                             lty = coord$linetype,
                             lineend = lineend
                           )
                         )
                       }
)

springGrob <- function(x0 = unit(0, "npc"),
                       y0 = unit(0, "npc"),
                       x1 = unit(1, "npc"),
                       y1 = unit(1, "npc"),
                       diameter = unit(0.1, "npc"),
                       tension = 0.75,
                       n = 50,
                       default.units = "npc",
                       name = NULL,
                       gp = gpar(),
                       vp = NULL) {

  # Use the default unit if the user does not specify one
  if (!is.unit(x0)) x0 <- unit(x0, default.units)
  if (!is.unit(x1)) x1 <- unit(x1, default.units)
  if (!is.unit(y0)) y0 <- unit(y0, default.units)
  if (!is.unit(y1)) y1 <- unit(y1, default.units)
  if (!is.unit(diameter)) diameter <- unit(diameter, default.units)

  # Return a gTree of class "spring"
  gTree(
    x0 = x0,
    y0 = y0,
    x1 = x1,
    y1 = y1,
    diameter = diameter,
    tension = tension,
    n = n,
    name = name,
    gp = gp,
    vp = vp,
    cl = "spring"
  )
}

#' @keywords internal
#' @export
#' @method makeContent spring
makeContent.spring <- function(x) {
  # Input: x = a gTree of springs. Essentially a list with the contents defined
  # just above, but also with: "children" (gList), "childrenOrder" (vector),
  # which are both currently length 0.

  # Convert position and diameter values absolute units.
  # These are all numeric vectors.
  x0 <- convertX(x$x0, "mm", valueOnly = TRUE)
  x1 <- convertX(x$x1, "mm", valueOnly = TRUE)
  y0 <- convertY(x$y0, "mm", valueOnly = TRUE)
  y1 <- convertY(x$y1, "mm", valueOnly = TRUE)
  diameter <- convertWidth(x$diameter, "mm", valueOnly = TRUE)

  # Leave tension and n untouched
  tension <- x$tension
  n <- x$n

  # Transform the input data to a data frame containing spring paths
  # Output: list of data.frames with columns: x, y, id
  springs <- lapply(seq_along(x0), function(i) {
    cbind(
      create_spring(
        x = x0[i],
        y = y0[i],
        xend = x1[i],
        yend = y1[i],
        diameter = diameter[i],
        tension = tension[i],
        n = n
      ),
      id = i
    )
  })

  # Output: data.frame with columns x, y, id
  springs <- do.call(rbind, springs)

  # Construct the grob
  spring_paths <- polylineGrob(
    x = springs$x,
    y = springs$y,
    id = springs$id,
    default.units = "mm",
    gp = x$gp # gp which was defined back in draw_panel() is eventually passed through to here.
  )
  # x$children which was a gList of length 0, is now defined as gList of polyline grobs.
  setChildren(x, gList(spring_paths))
}
