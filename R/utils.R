#' Convert a measured value to metric units
#'
#' @param ... (units) One of conversion_factors$parameter,
#'   e.g. SCr = units::set_units(88.4, "umol/l").
#'   Case insensitive.
#' @param param (char) Name of measurement, e.g. param = "SCr"
#' @param meas (units) Measurement
#'
#' @return (units) Converted measured value
#' @export
#'
#' @examples
#' as_metric(SCr = units::set_units(88.4, "umol/l"))
#' as_metric(param = "scr", meas = units::set_units(88.4, "umol/l"))
as_metric <- function(param = NULL, meas = NULL, ...) {
  ellipsis::check_dots_used()
  if (is.null(param) | is.null(meas)) {
    elli <- list(...)
    if (length(elli) == 0) return(NULL)  # as_metric(1) will return NULL, no warning
    param <- names(elli)[1]
    meas <- elli[[1]]
  }
  conversion <- conversion_factors[
    tolower(conversion_factors$parameter) == tolower(param),
  ]
  if (nrow(conversion) != 1) {
    stop(paste0("Unable to find conversion for `", param, "`"))
  }
  if (grepl("mol", units::deparse_unit(meas))) {
    units::set_units(
      meas * conversion$mol_weight,
      conversion$metric_units,
      mode = "standard"
    )
  } else {
    units::set_units(meas, conversion$metric_units, mode = "standard")
  }
}

conversion_factors <- tibble::tribble(
  ~parameter, ~metric_units, ~mol_weight, ~description,
  # 2012 AKI Guideline
  "SAmk", "ug/ml", 585.6, "Amikacin (serum, plasma)",
  "BUN", "mg/dl", 28.014, "Blood urea nitrogen",
  "SiCa", "mg/dl", 40.08, "Calcium, ionized (serum)",
  "SCr", "mg/dl", 113.120, "Creatinine (serum)",
  "CLcr", "ml/min", NA, "Creatinine clearance",
  "CGen", "ug/ml", 477.596, "Gentamicin (serum)",
  "Glc", "mg/dl", 180.156, "Glucose",
  "Lac", "mg/dl", 90.08, "Lactate (plasma)",
  "STob", "ug/ml", 467.5, "Tobramycin (serum, plasma)",
  "Urea", "mg/dl", 60.06, "Urea (plasma)" # changed from AKI 2012 Guideline
) %>%
  dplyr::mutate(mol_weight = units::set_units(mol_weight, "g/mol"))



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


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
