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

create_spring <- function(x,
                          y,
                          xend,
                          yend,
                          diameter = 1,
                          tension = 0.75,
                          n = 50) {

  # Validate the input arguments
  if (tension <= 0) {
    rlang::abort("`tension` must be larger than zero.")
  }
  if (diameter == 0) {
    rlang::abort("`diameter` can not be zero.")
  }
  if (n == 0) {
    rlang::abort("`n` must be greater than zero.")
  }

  # Calculate the direct length of the spring path
  length <- sqrt((x - xend)^2 + (y - yend)^2)

  # Calculate the number of revolutions and points we need
  n_revolutions <- length / (diameter * tension)
  n_points <- n * n_revolutions

  # Calculate the sequence of radians and the x and y offset values
  radians <- seq(0, n_revolutions * 2 * pi, length.out = n_points)
  x <- seq(x, xend, length.out = n_points)
  y <- seq(y, yend, length.out = n_points)

  # Create and return the transformed data frame
  data.frame(
    x = cos(radians) * diameter/2 + x,
    y = sin(radians) * diameter/2 + y
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
