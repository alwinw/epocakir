
#' Diagnosis of anemia from Hb concentration
#'
#' TODO Make consistent with other files
#'
#' @param data A data.frame X
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
  elli <- list(...)
  print(names(elli))
  if ("data" %in% names(elli)) {
    UseMethod("anemia", elli$data)
  } else if ("Hb" %in% names(elli)) {
    UseMethod("anemia", elli$Hb)
  } else {
    UseMethod("anemia")
  }
}


#' @rdname anemia
#' @export
anemia.numeric <- function(Hb,
                           age = NULL, birthday = NULL,
                           gender, ...) {
  print("numeric!")
  print(Hb)
}

#' @rdname anemia
#' @export
anemia.units <- function(Hb,
                         age = NULL, birthday = NULL,
                         gender, ...) {
  print("units!")
  print(Hb)
}

#' @rdname anemia
#' @export
anemia.default <- function(data = NULL, age = NULL, birthday = NULL,
                           gender, Hb, ...) {
  print("default!")
  print(data)
  # print(Hb)
}
