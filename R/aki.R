
#' Codify AKI
#'
#' @param sCr numeric
#' @param bCr numeric
#' @param UO numeric
#'
#' @return numeric
#' @export
#'
#' @examples
#' aki(1, 1, 1)
aki <- function(sCr, bCr, UO){
  return(sCr + UO)
}
