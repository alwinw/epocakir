.sCr2metric <- function(SCr) {
  if (grepl("mol", units::deparse_unit(SCr))) {
    return(units::set_units(SCr * units::set_units(113.120, "g/mol"), "mg/dl"))
  } else {
    return(units::set_units(SCr, "mg/dl"))
  }
}

#' Calculate age from DOB
#'
#' @param dob The date or vector of dates representing date(s) of birth.
#' @param age_on (Date) The date on which age is to be calculated.
#'   Defaults to today.
#' @param fun (function) The function to be applied to the age, e.g. floor.
#'   Defaults to NULL.
#' @param units (character) The units to measure age in, e.g. "years".
#'   Only used if `fun` is specified. Defaults to "years".
#' @param ... Further optional arguments that will be passed to `fun`
#'
#' @return (Duration) The age as a duration.
#' @export
#'
#' @examples
#' dob2age(lubridate::as_date("1990-01-01"))
#' dob2age(
#'   dob = c(
#'     lubridate::as_date("1990-01-01"),
#'     lubridate::as_date("1994-01-01"),
#'     lubridate::as_date("1998-01-01")
#'   ),
#'   age_on = lubridate::as_date("2002-12-31"),
#'   fun = floor
#' )
dob2age <- function(dob, age_on = lubridate::today(),
                    fun = NULL, units = "years", ...) {
  ellipsis::check_dots_used()
  age <- lubridate::as.duration(lubridate::interval(dob, age_on))
  if (!is.null(fun)) {
    age <- lubridate::duration(fun(as.numeric(age, units), ...), units)
  }
  return(age)
}


gender2factor <- function() {}
