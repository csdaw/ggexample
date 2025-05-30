draw_key_spring <- function(data, params, size) {
  springGrob(
    x0 = 0,
    y0 = 0,
    x1 = 1,
    y1 = 1,
    diameter = unit(data$diameter, "cm"),
    tension = data$tension,
    gp = gpar(
      col = alpha(data$colour %||% "black", data$alpha),
      lwd = (data$size %||% 0.5) * .pt,
      lty = data$linetype %||% 1
    ),
    vp = viewport(clip = "on")
  )
}
