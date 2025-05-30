scale_tension_continuous <- function(..., range = c(0.1, 1)) {
  continuous_scale(
    aesthetics = "tension",
    palette = scales::rescale_pal(range),
    ...
  )
}

scale_tension <- scale_tension_continuous

scale_tension_discrete <- function(...) {
  rlang::abort("Tension cannot be used with discrete data")
}
