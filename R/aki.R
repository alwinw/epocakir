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

#' @rdname aki
#' @export
aki.numeric <- function(x) {
  x + 1
}


#' @rdname aki
#' @export
aki.default <- function(x) {
  factor(x, levels = aki_stages)
}


aki_stages = c("AKI Stage 1", "AKI Stage 2", "AKI Stage 3")
