ckd_stages <- function() {
  dplyr::case_when(
    GFR >= units::set_units(90, "ml/min") ~ 1, # PER 1.73m2??
    GFR >= units::set_units(60, "ml/min") ~ 2,
    GFR >= units::set_units(30, "ml/min") ~ 3,
    GFR >= units::set_units(15, "ml/min") ~ 4,
    GFR >= units::set_units(0, "ml/min") ~ 5,
    TRUE ~ NA_real_
  )
}
