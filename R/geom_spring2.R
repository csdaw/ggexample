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

                      # Check that the user has specified sensible parameters
                      setup_params = function(data, params) {
                        if (is.null(params$n)) {
                          params$n <- 50
                        } else if (params$n <= 0) {
                          rlang::abort("Springs must be defined with `n` greater than 0")
                        }
                        params
                      },

                      # Check input data and return grobs
                      draw_panel = function(data,
                                            panel_params,
                                            coord,
                                            n = 50,
                                            lineend = "butt",
                                            na.rm = FALSE) {

                        # Remove missing data, returning early if all are missing
                        data <- remove_missing(
                          df = data,
                          na.rm = na.rm,
                          vars = c("x", "y", "xend", "yend", "linetype", "linewidth"),
                          name = "geom_spring"
                        )
                        if (is.null(data) || nrow(data) == 0) return(zeroGrob())

                        # Supply the coordinate system for the plot
                        if (!coord$is_linear()) {
                          rlang::warn(
                            "spring geom only works correctly on linear coordinate systems"
                          )
                        }
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
                      },

                      # Specify the default and required aesthetics
                      required_aes = c("x", "y", "xend", "yend"),
                      default_aes = aes(
                        colour = "black",
                        linewidth = 0.5,
                        linetype = 1L,
                        alpha = NA,
                        diameter = 0.35,
                        tension = 0.75
                      ),
                      draw_key = draw_key_spring
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

  # Convert position and diameter values absolute units
  x0 <- convertX(x$x0, "mm", valueOnly = TRUE)
  x1 <- convertX(x$x1, "mm", valueOnly = TRUE)
  y0 <- convertY(x$y0, "mm", valueOnly = TRUE)
  y1 <- convertY(x$y1, "mm", valueOnly = TRUE)
  diameter <- convertWidth(x$diameter, "mm", valueOnly = TRUE)

  # Leave tension and n untouched
  tension <- x$tension
  n <- x$n

  # Transform the input data to a data frame containing spring paths
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
  springs <- do.call(rbind, springs)

  # Construct the grob
  spring_paths <- polylineGrob(
    x = springs$x,
    y = springs$y,
    id = springs$id,
    default.units = "mm",
    gp = x$gp
  )
  setChildren(x, gList(spring_paths))
}
