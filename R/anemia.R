#' Diagnosis of anemia from Hb concentration
#'
#' KDIGO Clinical Practice Guideline for
#' Anemia in Chronic Kidney Disease
#' Volume 2 | Issue 4 | August (2) 2012
#'
#' \itemize{
#'   \item{Adults and children >15 years with CKD when the Hb concentration is
#'   <13.0 g/dl (<130 g/l) in males and
#'   <12.0 g/dl (<120 g/l) in females.}
#'   \item{Children with CKD if Hb concentration is
#'   <11.0 g/dl (<110 g/l) in children 0.5-5 years,
#'   <11.5 g/dl (115 g/l) in children 5-12 years,
#'   and <12.0 g/dl (120 g/l) in children 12-15 years.}
#' }
#'
#' See <https://kdigo.org/guidelines/anemia-in-ckd/> for more details.
#'
#' @param .data (data.frame) A data.frame, optional
#' @param Hb Hemoglobin concenration
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param age Age of patient
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param male Male or not
#'   column name, or vector of logical (TRUE/FALSE) if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return Anemia
#'   as logical `TRUE` or `FALSE`
#' @export
#'
#' @examples
#' anemia(anemia_pt_data, Hb = "Hb", age = "age", male = "male")
#'
#' anemia_pt_data %>%
#'   dplyr::mutate(anemia = anemia(Hb = Hb, age = age, male = male))
anemia <- function(...) {
  UseMethod("anemia")
}

#' @rdname anemia
#' @export
anemia.data.frame <- function(.data, Hb, age, male, ...) {
  ellipsis::check_dots_used()
  anemia(
    .data[[rlang::as_name(rlang::enquo(Hb))]],
    .data[[rlang::as_name(rlang::enquo(age))]],
    .data[[rlang::as_name(rlang::enquo(male))]]
  )
}

#' @rdname anemia
#' @export
anemia.units <- function(Hb, age, male, ...) {
  ellipsis::check_dots_used()
  anemia(
    as_metric(Hb = Hb, value_only = T),
    as_metric(age = age, value_only = T),
    male
  )
}

#' @rdname anemia
#' @export
anemia.numeric <- function(Hb, age, male, ...) {
  male <- as.logical(male)
  dplyr::case_when(
    age > 15 & male & Hb < 13.0 ~ TRUE,
    age > 15 & !male & Hb < 12.0 ~ TRUE,
    age > 12 & age <= 15 & Hb < 12.0 ~ TRUE,
    age > 5 & age <= 12 & Hb < 11.5 ~ TRUE,
    age > 0.5 & age <= 5 & Hb < 11.0 ~ TRUE,
    TRUE ~ FALSE
  )
}
