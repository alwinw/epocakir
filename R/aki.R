#' Codify AKI from Serum Creatinine and/or Urine Output
#'
#' Some additional details
#'
#' @param x numeric number
#'
#' @return
#'
#' @examples
#' @export
aki <- function(...) {
  UseMethod("aki")
}

.aki_stages <- factor(c("AKI Stage 1", "AKI Stage 2", "AKI Stage 3", "No AKI"))

#' @rdname aki
#' @export
aki.numeric <- function(SCr, UO, bCr = NULL, na.rm = FALSE) {
  if (is.null(SCr) & is.null(bCr)) {
    stop("One of SCr or UO must be provided")
  }

  if (is.null(bCr)) bCr <- min(SCr, na.rm = na.rm)

  dplyr::case_when(
    SCr > 3.0 * bCr ~ .aki_stages[3],
    TRUE ~ .aki_stages[4]
  )
}


#' @rdname aki
#' @export
aki.default <- function(x) {
  factor(x, levels = aki_stages)
}
