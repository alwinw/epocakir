#' Diagnosis of anemia from Hb concentration
#'
#' @param .data (data.frame) A data.frame, optional
#' @param Hb Hemoglobin concenration
#'   column name, or vector if `.data` not provided
#' @param Age Age of patient
#'   column name, or vector if `.data` not provided
#' @param male Male or not
#'   column name, or vector if `.data` not provided
#' @param ... Further optional arguments
#'
#' @return Anemia
#'   as logical `TRUE` or `FALSE`
#' @export
#'
#' @examples
#' print("todo")
anemia <- function(...) {
  UseMethod("anemia")
}

#' @rdname anemia
#' @export
anemia.default <- function(.data, Hb, age, male, ...) {
  ellipsis::check_dots_used()
  anemia(
    .data[[rlang::as_name(rlang::enquo(Hb))]],
    .data[[rlang::as_name(rlang::enquo(age))]]
  )
}

#' @rdname anemia
#' @export
anemia.units <- function(Hb, age, male, ...) {
  ellipsis::check_dots_used()
  anemia(
    as_metric(Hb = Hb, value_only = TRUE),
    as_metric(Age = Age, value_only = TRUE)
  )
}

#' @rdname anemia
#' @export
anemia.numeric <- function(Hb, age, male, ...) {
  male <- as.logical(male)
  dplyr::case_when(
    age > 15 & male & Hb < 13.0 ~ TRUE,
    age > 15 & !male & Hb < 12.0 ~ TRUE,
    age > 12 & age <= 15 & Hb < 12.0 ~ 2,
    age > 5 & age <= 12 & Hb < 11.5 ~ 3,
    age > 0 & age <= 0.5 & Hb < 11.0 ~ TRUE,
    TRUE ~ 0
  )
}
