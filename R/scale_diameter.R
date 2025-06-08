scale_diameter_continuous <- function(...,
                                      range = c(0.25, 0.7),
                                      unit = "cm") {
  range <- convertWidth(
    unit(range, unit),
    "cm",
    valueOnly = TRUE
  )
  continuous_scale(
    aesthetics = "diameter",
    palette = scales::rescale_pal(range),
    ...
  )
}

scale_diameter <- scale_diameter_continuous

scale_diameter_discrete <- function(...) {
  rlang::abort("Diameter cannot be used with discrete data")
}
