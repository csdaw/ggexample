#' Calculate the convex hull of a set of points
#'
#' @description This function is pulled from the ggplot2 book chapter
#' "Extending ggplot2".
#'
#' It currently uses `ggplot2::GeomPolygon` to actually draw anything.
#'
#' @inheritParams ggplot2::stat_density
#'
#' @returns Returns stuff...
#' @import ggplot2
#' @export
#'
#' @examples
#' # This will be an example.
#'
stat_chull <- function(mapping = NULL, data = NULL,
                       geom = "polygon", position = "identity",
                       na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       # browser()


                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     required_aes = c("x", "y")
)
