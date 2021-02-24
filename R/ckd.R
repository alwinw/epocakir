# Adult
GFR.SCr <- function() {
  kappa = dplyr::if_else(male, 0.9, 0.7)
  alpha = dplyr::if_else()
  141*min(SCr/kappa, 1)^alpha * max(SCr/kappa, 1)^-1.209 * 0.993^Age *
    dplyr::if_else(male, 1, 1.018) *
    dplyr::if_else(black, 1.159, 1)
}

GFR.SCysC <- function() {
  133*min(SCysC/0.8, 1)^-0.499 * max(SCysC/0.8, 1)^-1.328 * 0.996^Age *
    dplyr::if_else(male, 1, 0.932)
}

GFR.SCr.SCysC <- function() {
  135*min(SCr/kappa, 1)^alpha * max(SCr/kappa, 1)^-0.601 *
    min(SCysC/0.8, 1)^-0.375 * max(SCysC/0.8, 1)^-0.711 *
  0.995^Age *
  dplyr::if_else(male, 1, 0.969) *
  dplyr::if_else(black, 1.08, 1)
}

# Pediatric
GFR.SCr <- function() {
  41.3 * height/SCr |
    40.7 * (height/SCr) * XXX
}
