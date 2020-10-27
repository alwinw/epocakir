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
#' @param aki the variable name e.g. "aki_stages" to be used for the output
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

.aki_bCr <- function(SCr, bCr) {
  dplyr::case_when(
    as_metric_SCr(SCr) >= units::set_units(4.0, "mg/dl") ~ .aki_stages[3],
    SCr >= 3.0 * bCr ~ .aki_stages[3],
    SCr >= 2.0 * bCr ~ .aki_stages[2],
    SCr >= 1.5 * bCr ~ .aki_stages[1],
    TRUE ~ .aki_stages[length(.aki_stages)]
  )
}

.aki_UO <- function(dttm, UO) {
  print(UO)
}

.aki_cr_ch <- function(data, SCr, dttm, pt_id) {
  print(SCr)
}

#' @rdname aki
#' @export
aki.numeric <- function(SCr,
                        bCr = NULL,
                        units = "umol/l", na.rm = FALSE, ...) {
  SCr <- units::as_units(SCr, units)
  if (is.null(bCr)) {
    bCr <- min(SCr, na.rm = na.rm) # Must be run after as_units(SCr, ...)
  }
  else {
    bCr <- units::as_units(bCr, units)
  }
  .aki_bCr(SCr, bCr)
}

#' @rdname aki
#' @export
aki.units <- function(SCr,
                      bCr = NULL,
                      na.rm = FALSE, ...) {
  if (is.null(bCr)) bCr <- min(SCr, na.rm = na.rm)
  .aki_bCr(SCr, bCr)
}


#' @rdname aki
#' @export
aki.ts <- function(SCr, UO, units = "umol/l", ...) {

}

#' @rdname aki
#' @export
aki.default <- function(data,
                        SCr = NULL, bCr = NULL, UO = NULL,
                        aki = "aki",
                        units = list("SCr" = "umol/l"), na.rm = FALSE, ...) {
  # TODO check if aki is an existing company
  # Check SCr or bCr are given!
  # Calc bCr if not given
  # TODO consider fuctionalising aki.bCr, etc

  data %>%
    dplyr::mutate(
      "{aki}.bCr" := .aki_bCr(!!as.name(SCr), !!as.name(bCr))
    ) %>%
    dplyr::mutate(
      "{aki}.cr_ch" := !!as.name(SCr) * 2
    )
}


#' Generate creatinine changes
#'
#' @param data A data.frame of datetime and creatinine changes
#'
#' @param SCr The variable name for serum creatinine column (units)
#' @param dttm The variable name for the datetime column (POSIXct)
#' @param pt_id The variable name for the patient id (optional, character)
#'
#' @return A data.frame of without input variable names and creatinine changes
#' @export
#'
#' @examples
#' data_ <- data.frame(
#'   pt_id_ = c(rep("pt1", 3 + 3), rep("pt2", 3)),
#'   dttm_ = c(
#'     seq(
#'       lubridate::as_datetime("2020-10-18 09:00:00", tz = "Australia/Melbourne"),
#'       lubridate::as_datetime("2020-10-20 09:00:00", tz = "Australia/Melbourne"),
#'       length.out = 3
#'     ),
#'     seq(
#'       lubridate::as_datetime("2020-10-23 09:00:00", tz = "Australia/Melbourne"),
#'       lubridate::as_datetime("2020-10-25 21:00:00", tz = "Australia/Melbourne"),
#'       length.out = 3
#'     ),
#'     seq(
#'       lubridate::as_datetime("2020-10-18 10:00:00", tz = "Australia/Melbourne"),
#'       lubridate::as_datetime("2020-10-19 10:00:00", tz = "Australia/Melbourne"),
#'       length.out = 3
#'     )
#'   ),
#'   SCr_ = c(
#'     units::set_units(seq(2.0, 3.0, by = 0.5), "mg/dl"),
#'     units::set_units(seq(3.5, 4.0, by = 0.25), "mg/dl"),
#'     units::set_units(seq(3.3, 3.5, by = 0.10), "mg/dl")
#'   ),
#'   bCr_ = c(
#'     rep(units::set_units(1.8, "mg/dl"), 3 + 3),
#'     rep(units::set_units(3.0, "mg/dl"), 3)
#'   )
#' )
#' data <- data_[sample(nrow(data_)), ]
#'
#' generate_cr_ch(data, SCr = "SCr_", dttm = "dttm_", pt_id = "pt_id_")
#' @importFrom rlang .data
#' @importFrom rlang `:=`
generate_cr_ch <- function(data, SCr, dttm, pt_id = NULL) {
  # TODO Consider saving current grouping settings e.g. dplyr::group_data()
  # Ref: https://tidyeval.tidyverse.org/dplyr.html
  data_gr <- data[, c(SCr, dttm)]
  if (is.null(pt_id)) {
    data_gr$pt_id <- "pt"
  } else {
    data_gr$pt_id <- data[[pt_id]]
  }
  colnames(data_gr) <- c("SCr", "dttm", "pt_id")
  data_gr <- data_gr %>%
    dplyr::group_by(.data$pt_id, .add = FALSE) %>%
    dplyr::arrange(.data$pt_id, .data$dttm) %>%
    unique() %>%
    dplyr::mutate(
      admin = cumsum(
        (dttm - dplyr::lag(dttm, default = lubridate::as_date(0))) >=
          lubridate::duration(hours = 48)
      )
    ) %>%
    dplyr::group_by(.data$admin, .add = TRUE)
  # check for nrow < 2
  data_n <- data_gr %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n_1 = cumsum(dplyr::lag(.data$n, default = 0))) %>%
    dplyr::rowwise() %>%
    dplyr::do(data.frame(.data$n_1 + t(utils::combn(.data$n, 2)))) %>% # TODO do() superseded, replace
    dplyr::arrange(.data$X2, dplyr::desc(.data$X1))
  # consider a more dplyr version e.g. pivot_longer (X1, X2) then use summarise and diff
  T1 <- data_gr[data_n$X1, ]
  T2 <- data_gr[data_n$X2, ]
  # The patient id should also match, remove after testing
  if (!all.equal(T1[c("pt_id", "admin")], T2[c("pt_id", "admin")])) {
    warning("Unexpected mismatch in patient ids")
  }
  data_c <- data.frame(
    pt_id = T1$pt_id,
    admin = T1$admin,
    dttm = T2$dttm,
    SCr = T2$SCr,
    D.SCr = T2$SCr - T1$SCr,
    D.dttm = T2$dttm - T1$dttm
  ) %>%
    dplyr::filter(.data$D.dttm <= lubridate::duration(hours = 48)) %>%
    dplyr::select(.data$pt_id, .data$dttm:.data$D.dttm) %>%
    dplyr::rename(!!dttm := .data$dttm, !!SCr := .data$SCr)
  if (is.null(pt_id)) {
    data_c$pt_id <- NULL
  } else {
    data_c <- dplyr::rename(data_c, !!pt_id := .data$pt_id)
  }
  return(data_c)
}

