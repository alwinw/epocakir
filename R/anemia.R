
#' Diagnosis of anemia from Hb concentration
#'
#' TODO Make consistent with other files
#'
#' @param data A data.frame X
#' @param age The variable name, e.g. "age" or X
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
anemia.numeric <- function(Hb, age, gender, ...) {
  print("numeric!")
  print(Hb)
}

#' @rdname anemia
#' @export
anemia.units <- function(Hb, age, gender, ...) {
  dplyr::case_when(
    age > lubridate::duration(years = 15) & Hb < units::set_units(13.0, "g/dl") ~ 1,
    age > lubridate::duration(years = 12) & Hb < units::set_units(12.0, "g/dl") ~ 2,
    age > lubridate::duration(years = 5) & Hb < units::set_units(11.5, "g/dl") ~ 3,
    age > lubridate::duration(years = 0.5) & Hb < units::set_units(11.0, "g/dl") ~ 4,
    TRUE ~ 0
  )
}

#' @rdname anemia
#' @export
anemia.default <- function(data = NULL, Hb, age, gender, ...) {
  print("default!")
  print(data)
  # print(Hb)
}
