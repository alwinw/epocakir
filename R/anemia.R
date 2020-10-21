
#' Diagnosis of anemia from Hb concentration
#'
#' TODO Make consistent with other files
#'
#' @param age The variable name, e.g. "age" or X
#' @param birthday The variable name, e.g.  "birthday" or X
#' @param gender The variable name, e.g. "gender" or X
#' @param Hb The variable name e.g. "Hb" or X
#' @param ... Further optional arguments that will be passed to method.
#'
#' @return A dataframe or X
#' @export
#'
#' @examples
#' anemia(1:10)
anemia <- function(...) {
  ellipsis::check_dots_used()
  UseMethod("anemia")
}


#' @rdname anemia
#' @export
anemia.numeric <- function(age, birthday, gender, Hb, ...) {

}

#' @rdname anemia
#' @export
anemia.units <- function(age, birthday, gender, Hb, ...) {

}
