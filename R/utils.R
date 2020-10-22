.sCr2metric <- function(SCr) {
  if (grepl("mol", units::deparse_unit(SCr))) {
    return(units::set_units(SCr * units::set_units(113.120, "g/mol"), "mg/dl"))
  } else {
    return(units::set_units(SCr, "mg/dl"))
  }
}

.dob2age <- function(dob, age_on = lubridate::today(), units = "years", fun = NULL) {
  age = lubridate::as.duration(lubridate::interval(dob, age_on), units)
  if (!is.null(fun)) fun(age)
  return(age)
}
