.sCr2metric <- function(SCr) {
  if (grepl("mol", units::deparse_unit(SCr))) {
    return(units::set_units(SCr * units::set_units(113.120, "g/mol"), "mg/dl"))
  } else {
    return(units::set_units(SCr, "mg/dl"))
  }
}

#' @importFrom rlang .data
.generate_cr_ch <- function(data, SCr, dttm, pt_id) {
  # Issues with R CMD CHECK when trying to
  # {if (l) dplyr::group_by(., dplyr::across(dplyr::all_of(pt_id))) else ...}
  # TODO Consider saving current grouping settings

  if (is.null(pt_id)) {
    data$.pt_id <- data[[pt_id]]
  } else {
    data$.pt_id <- "pt"
  }

  data_g <- data %>%
    dplyr::group_by(.data$.pt_id, .add = FALSE) %>%
    dplyr::select(dplyr::all_of(c(pt_id, dttm, SCr))) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(dttm)), .by_group = TRUE) %>%
    unique()

  # check for nrow < 2

  combns <- utils::combn(nrow(data_g), 2)

  return(data_g)

  # if (nrow(cr_ts) < 2) {
  #   return(data.frame(
  #     DateTime_Pathology_Result = as_datetime(NA_real_),
  #     del_t_ch  = as.duration(NA_real_),
  #     del_t_aki = as.duration(NA_real_),
  #     del_cr    = NA_real_,
  #     cr      = NA_real_
  #   ))
  # }
  # Consider filtering out ones post AKI here?

  # combns <- combn(nrow(cr_ts), 2)
  # Ti_1 = cr_ts[combns[1,],]
  # Ti   = cr_ts[combns[2,],]
  #
  # if(AKI_ICU == 0 | is.na(AKI_ICU)) {
  #   del_t_aki = rep(as.duration(NA_real_), nrow(Ti))
  # } else {
  #   del_t_aki = as.duration(DateTime_AKI_Dx - Ti$Pathology_Result_DTTM)
  # }
  #
  # return(data.frame(
  #   DateTime_Pathology_Result = Ti$Pathology_Result_DTTM,
  #   del_t_ch  = as.duration(Ti$Pathology_Result_DTTM - Ti_1$Pathology_Result_DTTM),
  #   del_t_aki = del_t_aki,
  #   del_cr    = Ti$Creatinine_level - Ti_1$Creatinine_level,
  #   cr      = Ti$Creatinine_level
  # ))
}

#' Codify AKI from Serum Creatinine and/or Urine Output
#'
#' Using KDIGO Clinical Practice Guideline for Acute Kidney Injury
#' Volume 2 | Issue 1 | March 2012
#'
#' Provided a series of Serum Creatinine readings and/or Urine Output, aki()
#' calculates whether or not a patient has AKI. The staging (1, 2, 3) of AKI is
#' also calculated
#'
#' @param data A data.frame with the data needed for serum creatinine (SCr)
#'   and/or urine output (UO)
#' @param SCr The variable name, e.g. "cr", to be used for determining
#'   AKI based on creatinine level. A numeric vector or time series of
#'   serum creatinine values if the data argument is unused
#' @param UO The variable name, e.g. "urine", to be used for determining
#'   AKI based on urine output. A numeric vector or time series of
#'   urine output values if the data argument is unused
#' @param bCr The variable name, e.g. "baseline_cr", to be used for determining
#'   AKI based on urine output. A single numeric value of
#'   baseline creatinine if the data argument is unused
#' @param units (character) Units of SCr and UO metric (mg/dl) or SI (umol/l)
#'   #TOFIX, consider changing to a list
#' @param na.rm (logical) If TRUE, missing values are removed
#' @param ... Further optional arguments that will be passed to method.
#'
#' @examples
#' aki(seq(60, 200, by = 20))
#' aki(SCr = seq(60, 200, by = 20), bCr = 50)
#' @export
aki <- function(...) {
  ellipsis::check_dots_used()
  UseMethod("aki")
}

.aki_stages <- factor(c("AKI Stage 1", "AKI Stage 2", "AKI Stage 3", "No AKI"))

#' @rdname aki
#' @export
aki.numeric <- function(SCr, bCr = NULL, units = "umol/l", na.rm = FALSE, ...) {
  SCr <- units::as_units(SCr, units)
  if (is.null(bCr)) {
    bCr <- min(SCr, na.rm = na.rm) # Must be run after as_units(SCr, ...)
  }
  else {
    bCr <- units::as_units(bCr, units)
  }
  aki_stages <- aki.units(SCr = SCr, bCr = bCr, na.rm = na.rm)
  return(aki_stages)
}

#' @rdname aki
#' @export
aki.units <- function(SCr, bCr = NULL, na.rm = FALSE, ...) {
  if (is.null(bCr)) bCr <- min(SCr, na.rm = na.rm)
  aki_stages <- dplyr::case_when(
    .sCr2metric(SCr) >= units::set_units(4.0, mg / dl) ~ .aki_stages[3],
    SCr >= 3.0 * bCr ~ .aki_stages[3],
    SCr >= 2.0 * bCr ~ .aki_stages[2],
    SCr >= 1.5 * bCr ~ .aki_stages[1],
    TRUE ~ .aki_stages[length(.aki_stages)]
  )
  return(aki_stages)
}


#' @rdname aki
#' @export
aki.ts <- function(SCr, UO, units = "umol/l", ...) {

}

#' @rdname aki
#' @export
aki.default <- function(data, SCr, bCr, units = list("SCr" = "umol/l"), na.rm = FALSE, ...) {
  predictor <- rlang::as_name(rlang::enquo(SCr))
  factor(data, levels = .aki_stages)
}
