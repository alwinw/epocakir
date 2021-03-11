#' GFR Estimation
#'
#' - 2009 CKD-EPI creatinine equation
#' - 2012 CKD-EPI cystatin C equation
#' - 2012 CKD-EPI creatinine-cystatin C equation
#' - Pediatric equations
#'
#' @param SCr (units) Serum creatinine, converted to mg/dl
#' @param SCysC (units) Serum cystatin C, converted to mg/l
#' @param Age (units) Age of patient, converted to years
#' @param height (units) Height of patient, converted to m
#' @param BUN (units) Blood urea nitrogen, converted to mg/dl
#' @param male (logical) Male or not
#' @param black (logical) Black race or not
#' @param pediatric (logical) Paediatric or not
#'
#' @return (units) Estimated glomerular filtration rate (eGFR) in ml/min/1.73m2
#' @export
#'
#' @examples
#' print("todo")
eGFR <- function(SCr = NULL,
                 SCysC = NULL,
                 Age = NULL,
                 height = NULL,
                 BUN = NULL,
                 male = FALSE,
                 black = FALSE,
                 pediatric = FALSE) {
  dplyr::case_when(
    !pediatric & !is.null(SCr) & is.null(SCysC) ~ eGFR_adult_SCr(SCr, Age, male, black),
    !pediatric & is.null(SCr) & !is.null(SCysC) ~ eGFR_adult_SCysC(SCysC, Age, male),
    !pediatric & !is.null(SCr) & !is.null(SCysC) ~ eGFR_adult_SCr_SCysC(SCr, SCysC, Age, male, black),
    pediatric & !is.null(SCr) & !is.null(height) & is.null(BUN) & is.null(SCysC) ~ eGFR_child_SCr(SCr, height),
    pediatric & !is.null(SCr) & !is.null(height) & !is.null(BUN) & is.null(SCysC) ~ eGFR_child_SCr_BUN(SCr, height, BUN),
    pediatric & is.null(SCr) & !is.null(SCysC) ~ GFR.child.SCysC(SCysC),
    TRUE ~ NA_real_
  )
}


#' @rdname eGFR
#' @export
eGFR_adult_SCr <- function(SCr, Age, male, black) {
  SCr <- as_metric(SCr = SCr, value_only = TRUE)
  Age <- as_metric(Age = Age, value_only = TRUE)
  male <- as.logical(male)
  black <- as.logical(black)
  kappa <- dplyr::if_else(!male, 0.7, 0.9)
  alpha <- dplyr::if_else(!male, -0.329, -0.411)
  eGFR <- 141 * pmin(SCr / kappa, 1)^alpha * pmax(SCr / kappa, 1)^-1.209 * 0.993^Age *
    dplyr::if_else(male, 1, 1.018) *
    dplyr::if_else(black, 1.159, 1)
  units::set_units(eGFR, "mL/min/1.73m2")
}


#' @rdname eGFR
#' @export
eGFR_adult_SCysC <- function(SCysC, Age, male) {
  SCysC <- as_metric(SCysC = SCysC, value_only = TRUE)
  Age <- as_metric(Age = Age, value_only = TRUE)
  male <- as.logical(male)
  eGFR <- 133 * pmin(SCysC / 0.8, 1)^-0.499 * pmax(SCysC / 0.8, 1)^-1.328 * 0.996^Age *
    dplyr::if_else(male, 1, 0.932)
  units::set_units(eGFR, "mL/min/1.73m2")
}


#' @rdname eGFR
#' @export
eGFR_adult_SCr_SCysC <- function(SCr, SCysC, Age, male, black) {
  SCr <- as_metric(SCr = SCr, value_only = TRUE)
  SCysC <- as_metric(SCysC = SCysC, value_only = TRUE)
  Age <- as_metric(Age = Age, value_only = TRUE)
  male <- as.logical(male)
  black <- as.logical(black)
  kappa <- dplyr::if_else(!male, 0.7, 0.9)
  alpha <- dplyr::if_else(!male, -0.248, -0.207)
  eGFR <- 135 * pmin(SCr / kappa, 1)^alpha * pmax(SCr / kappa, 1)^-0.601 *
    pmin(SCysC / 0.8, 1)^-0.375 * pmax(SCysC / 0.8, 1)^-0.711 *
    0.995^Age *
    dplyr::if_else(male, 1, 0.969) *
    dplyr::if_else(black, 1.08, 1)
  units::set_units(eGFR, "mL/min/1.73m2")
}


#' @rdname eGFR
#' @export
eGFR_child_SCr <- function(SCr, height) {
  SCr <- as_metric(SCr = SCr, value_only = TRUE)
  height <- as_metric(height = height, value_only = TRUE)
  eGFR <- 41.3 * (height / SCr)
  units::set_units(eGFR, "mL/min/1.73m2")
}


#' @rdname eGFR
#' @export
eGFR_child_SCr_BUN <- function(SCr, height, BUN) {
  SCr <- as_metric(SCr = SCr, value_only = TRUE)
  height <- as_metric(height = height, value_only = TRUE)
  BUN <- as_metric(BUN = BUN, value_only = TRUE)
  eGFR <- 40.7 * (height / SCr)^0.64 * (30 / BUN)^0.202
  units::set_units(eGFR, "mL/min/1.73m2")
}


#' @rdname eGFR
#' @export
eGFR_child_SCysC <- function(SCysC) {
  SCysC <- as_metric(SCysC = SCysC, value_only = TRUE)
  eGFR <- 70.69 * (SCysC)^-0.931
  units::set_units(eGFR, "mL/min/1.73m2")
}

GFR_stages <- factor(c("G1", "G2", "G3a", "G3b", "G4", "G5"), ordered = TRUE)

GFR_staging <- function() {
  dplyr::case_when(
    GFR >= units::set_units(90, "ml/min") ~ GFR_stages[1],
    GFR >= units::set_units(60, "ml/min") ~ GFR_stages[2],
    GFR >= units::set_units(45, "ml/min") ~ GFR_stages[3],
    GFR >= units::set_units(30, "ml/min") ~ GFR_stages[4],
    GFR >= units::set_units(15, "ml/min") ~ GFR_stages[5],
    GFR >= units::set_units(0, "ml/min") ~ GFR_stages[5],
    TRUE ~ NA_real_
  )
}

Albuminuria_stages <- factor(c("A1", "A2", "A3"), ordered = TRUE)

Albuminuria_levels_AER <- function() {
  dplyr::case_when(
    AER > units::set_units(300, "mg/day") ~ Albuminuria_stages[3],
    AER > units::set_units(30, "mg/day") ~ Albuminuria_stages[2],
    AER > units::set_units(0, "mg/day") ~ Albuminuria_stages[1],
    TRUE ~ NA_real_
  )
}

Albuminuria_levels_ACR <- function() {
  dplyr::case_when(
    ACR > units::set_units(30, "mg/g") ~ Albuminuria_stages[3],
    ACR > units::set_units(3, "mg/g") ~ Albuminuria_stages[2],
    ACR > units::set_units(0, "mg/g") ~ Albuminuria_stages[1],
    TRUE ~ NA_real_
  )
}
