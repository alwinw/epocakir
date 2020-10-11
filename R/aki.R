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
aki <- function(x, ...) {
  UseMethod("aki", x)
}

.aki_stages <- factor(c("AKI Stage 1", "AKI Stage 2", "AKI Stage 3", "No AKI"))

#' @rdname aki
#' @export
aki.numeric <- function(SCr, bCr = NULL, na.rm = FALSE) {
  if (is.null(bCr)) bCr <- min(SCr, na.rm = na.rm)
  aki_stages <- dplyr::case_when(
    SCr >= 3.0 * bCr ~ .aki_stages[3],
    SCr >= 2.0 * bCr ~ .aki_stages[2],
    SCr >= 1.5 * bCr ~ .aki_stages[1],
    TRUE ~ .aki_stages[length(.aki_stages)]
  )
  return(aki_stages)
}

#' @rdname aki
#' @export
aki.ts <- function(ts_data) {

}


#' @rdname aki
#' @export
aki.default <- function(x) {
  factor(x, levels = .aki_stages)
}
