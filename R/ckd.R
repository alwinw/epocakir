GFR.adult.SCr <- function(SCr, Age, male, black) {
  kappa <- dplyr::if_else(!male, 0.7, 0.9)
  alpha <- dplyr::if_else(!male, -0.329, -0.411)
  141 * min(SCr / kappa, 1)^alpha * max(SCr / kappa, 1)^-1.209 * 0.993^Age *
    dplyr::if_else(male, 1, 1.018) *
    dplyr::if_else(black, 1.159, 1)
}


GFR <- function(SCr = NULL,
                SCysC = NULL,
                Age = NULL,
                height = NULL,
                BUN = NULL,
                male = FALSE,
                black = FALSE,
                pediatric = FALSE) {
  dplyr::case_when(
    !pediatric & !is.null(SCr) & is.null(SCysC) ~ GFR.adult.SCr(SCr, Age, male, black),
    TRUE ~ NA_real_
  )
}



# Adult
GFR.SCr <- function() {

  # Then set units of GFR
}


GFR.SCysC <- function() {
  133 * min(SCysC / 0.8, 1)^-0.499 * max(SCysC / 0.8, 1)^-1.328 * 0.996^Age *
    dplyr::if_else(male, 1, 0.932)
}

GFR.SCr.SCysC <- function() {
  135 * min(SCr / kappa, 1)^alpha * max(SCr / kappa, 1)^-0.601 *
    min(SCysC / 0.8, 1)^-0.375 * max(SCysC / 0.8, 1)^-0.711 *
    0.995^Age *
    dplyr::if_else(male, 1, 0.969) *
    dplyr::if_else(black, 1.08, 1)
}

# Pediatric
GFR.SCr <- function() {
  41.3 * height / SCr |
    40.7 * (height / SCr)^0.4 * (30 / BUN)^0.202
}

GFR.SCysC <- function() {
  70.69 * (SCysC)^-0.931
}

GFR.levels <- function() {
  dplyr::case_when(
    GFR >= units::set_units(90, "ml/min") ~ "G1", # PER 1.73m2??
    GFR >= units::set_units(60, "ml/min") ~ "G2",
    GFR >= units::set_units(45, "ml/min") ~ "G3a",
    GFR >= units::set_units(30, "ml/min") ~ "G3b",
    GFR >= units::set_units(15, "ml/min") ~ "G4",
    GFR >= units::set_units(0, "ml/min") ~ "G5",
    TRUE ~ NA_real_
  )
}

Albuminuria <- function() {
  dplyr::case_when(
    AER < units::set_units(30, "mg/day") ~ "A1"
    # Add other cases
  )
}
