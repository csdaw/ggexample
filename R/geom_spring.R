#' Geom Spring
#'
#' @description This will be the description.
#'
#' @inheritParams ggplot2::geom_path
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

                      setup_data = function(data, params) {
                        if (anyDuplicated(data$group)) {
                          data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
                        }
                        data
                      },

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

                      required_aes = c("x", "y", "xend", "yend"),
                      optional_aes = c("diameter", "tension")
)

GeomSpring <- ggproto("GeomSpring", Geom,

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

                      # Transform the data inside the draw_panel() method
                      draw_panel = function(data,
                                            panel_params,
                                            coord,
                                            n = 50,
                                            arrow = NULL,
                                            lineend = "butt",
                                            linejoin = "round",
                                            linemitre = 10,
                                            na.rm = FALSE) {

                        # browser()

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
                      },

                      # Specify the default and required aesthetics
                      required_aes = c("x", "y", "xend", "yend"),
                      default_aes = aes(
                        colour = "black",
                        linewidth = 0.5,
                        linetype = 1L,
                        alpha = NA,
                        diameter = 1,
                        tension = 0.75
                      )
)
