#' Calculate the convex hull of a set of points
#'
#' @description This function is pulled from the ggplot2 book chapter
#' "Extending ggplot2".
#'
#' It currently uses `ggplot2::GeomPolygon` to actually draw anything.
#'
#' @inheritParams ggplot2::stat_density
#' @param geom Override the default connection between `stat_chull()` and
#'   `geom_polygon()`. For more information about overriding these connections,
#'   see how the [stat][ggplot2::layer_stats] and [geom][ggplot2::layer_geoms] arguments work.
#'
#' @returns Returns stuff...
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
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     required_aes = c("x", "y")
)
