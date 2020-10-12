#' Codify AKI from Serum Creatinine and/or Urine Output
#'
#' Using KDIGO Clinical Practice Guideline for Acute Kidney Injury
#' Volume 2 | Issue 1 | March 2012
#'
#' Provided a series of Serum Creatinine readings and/or Urine Output, aki()
#' calculates whether or not a patient has AKI. The staging (1, 2, 3) of AKI is
#' also calculated
#'
#' @param data A data.frame with the data needed for serum creatinine (SCr)
#'   and/or urine output (UO)
#' @param SCr The variable name, e.g. "cr", to be used for determining
#'   AKI based on creatinine level. A numeric vector or time series of
#'   serum creatinine values if the data argument is unused
#' @param UO The variable name, e.g. "urine", to be used for determining
#'   AKI based on urine output. A numeric vector or time series of
#'   urine output values if the data argument is unused
#' @param bCr The variable name, e.g. "baseline_cr", to be used for determining
#'   AKI based on urine output. A single numeric value of
#'   baseline creatinine if the data argument is unused
#' @param units (character) Units of SCr and UO metric (mg/dl) or SI (umol/l)
#'   #TOFIX
#' @param na.rm (logical) If TRUE, missing values are removed
#' @param ... Further optional arguments that will be passed to method.
#'
#' @examples
#' aki(seq(60, 200, by = 20))
#' aki(SCr = seq(60, 200, by = 20), bCr = 50)
#' @export
aki <- function(...) {
  ellipsis::check_dots_used()
  UseMethod("aki")
}

.aki_stages <- factor(c("AKI Stage 1", "AKI Stage 2", "AKI Stage 3", "No AKI"))


#' @rdname aki
#' @export
aki.numeric <- function(SCr, bCr = NULL, units = "umol/l", na.rm = FALSE, ...) {
  if (is.null(bCr)) bCr <- min(SCr, na.rm = na.rm)
  aki_stages <- dplyr::case_when(
    # Need one for SCr > X
    SCr >= 3.0 * bCr ~ .aki_stages[3],
    SCr >= 2.0 * bCr ~ .aki_stages[2],
    SCr >= 1.5 * bCr ~ .aki_stages[1],
    TRUE ~ .aki_stages[length(.aki_stages)]
  )
  return(aki_stages)
}

#' @rdname aki
#' @export
aki.ts <- function(SCr, UO, units = "umol/l", ...) {

}

#' @rdname aki
#' @export
aki.default <- function(data, SCr, bCr, units = list("SCr" = "umol/l"), na.rm = FALSE, ...) {
  factor(data, levels = .aki_stages)
}
