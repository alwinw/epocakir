
aki_stages <- factor(c("AKI Stage 1", "AKI Stage 2", "AKI Stage 3"), ordered = TRUE)

aki_staging <- function() {}


#' AKI Staging based on baseline creatinine
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCr Serum creatinine
#'   column name, or vector if `.data` not provided
#' @param bCr Baseline creatinine
#'   column name, or vector if `.data` not provided
#' @param ... Further optional arguments
#'
#' @return (ordered factor) AKI stages
#' @export
#'
#' @examples
#' print("todo")
aki_bCr <- function(...) {
  UseMethod("aki_bCr")
}

#' @rdname aki_bCr
#' @export
aki_bCr.default <- function(.data, SCr, bCr, ...) {
  ellipsis::check_dots_used()
  aki_bCr(
    .data[[rlang::as_name(rlang::enquo(SCr))]],
    .data[[rlang::as_name(rlang::enquo(bCr))]]
  )
}

#' @rdname aki_bCr
#' @export
aki_bCr.units <- function(SCr, bCr, ...) {
  ellipsis::check_dots_used()
  aki_bCr(
    as_metric(SCr = SCr, value_only = T),
    as_metric(SCr = bCr, value_only = T)
  )
}

#' @rdname aki_bCr
#' @export
aki_bCr.numeric <- function(SCr, bCr, ...) {
  ellipsis::check_dots_used()
  dplyr::case_when(
    SCr >= 4.0 ~ aki_stages[3],
    SCr >= 3.0 * bCr ~ aki_stages[3],
    SCr >= 2.0 * bCr ~ aki_stages[2],
    SCr >= 1.5 * bCr ~ aki_stages[1],
    TRUE ~ NA_integer_
  )
}


#' AKI Staging based on changes in creatinine
#'
#' @param .data (data.frame) A data.frame, optional
#' @param dttm DateTime
#'   column name, or vector if `.data` not provided
#' @param SCr Serum creatinine
#'   column name, or vector if `.data` not provided
#' @param pt_id Patient ID
#'   column name, or vector if `.data` not provided
#' @param ... Further optional arguments
#'
#' @return (ordered factor) AKI stages
#' @export
#'
#' @examples
#' print("todo")
aki_SCr <- function(...) {
  UseMethod("aki_SCr")
}

#' @rdname aki_SCr
#' @export
aki_SCr.default <- function(.data, SCr, dttm, pt_id, ...) {
  ellipsis::check_dots_used()
  aki_SCr(
    .data[[rlang::as_name(rlang::enquo(SCr))]],
    .data[[rlang::as_name(rlang::enquo(dttm))]],
    .data[[rlang::as_name(rlang::enquo(pt_id))]]
  )
  # TODO: Add column names back in.
  # Not sure if this is required if just returning a vector
}

#' @rdname aki_SCr
#' @export
aki_SCr.units <- function(SCr, dttm, pt_id, ...) {
  ellipsis::check_dots_used()
  aki_SCr(
    as_metric(SCr = SCr, value_only = T),
    dttm,
    pt_id
  )
}

#' @rdname aki_SCr
#' @export
aki_SCr.numeric <- function(SCr, dttm, pt_id, ...) {
  ellipsis::check_dots_used()
  SCr_changes <- combn_changes(dttm, SCr, pt_id) %>%
    dplyr::mutate(
      .aki = dplyr::case_when(
        D.val >= 0.3 & D.dttm < lubridate::duration(hours = 48) ~ aki_stages[1],
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::select(.data$pt_id, .data$dttm, .data$.aki)
  # Do I need a summarise here?

  dplyr::left_join(
    tibble::tibble(pt_id = pt_id, dttm = dttm),
    SCr_changes,
    by = c("pt_id", "dttm")
  ) #%>%
    #dplyr::pull(.data$.aki)
}


aki_UO <- function(...) {
  UseMethod("aki_UO")
}

aki_UO.default <- function(.data, dttm, UO, ...) {
  ellipsis::check_dots_used()
  aki_UO(
    .data[[rlang::as_name(rlang::enquo(dttm))]],
    .data[[rlang::as_name(rlang::enquo(UO))]]
  )
}

aki_UO.units <- function(dttm, UO, ...) {
  aki_UO(
    dttm,
    as_metric(UO = UO, value_only = T)
  )
}

aki_UO.numeric <- function(dttm, UO, ...) {
  print("todo")
  # TODO need to generate individual UO changes
  # then determine average urine output
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
#' @param aki the variable name e.g. "aki_stages" to be used for the output
#' @param units (character) Units of SCr and UO metric (mg/dl) or SI (umol/l)
#'   #TOFIX, consider changing to a list
#' @param na.rm (logical) If TRUE, missing values are removed
#' @param ... Further optional arguments that will be passed to method.
#'
#' @examples
#' # aki(seq(60, 200, by = 20))
#' # aki(SCr = seq(60, 200, by = 20), bCr = 50)
#' @importFrom rlang .data
#' @importFrom rlang `:=`
#' @export
aki <- function(...) {
  ellipsis::check_dots_used()
  UseMethod("aki")
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
  aki_bCr(SCr, bCr)
}

#' @rdname aki
#' @export
aki.units <- function(SCr,
                      bCr = NULL,
                      na.rm = FALSE, ...) {
  if (is.null(bCr)) bCr <- min(SCr, na.rm = na.rm)
  aki_bCr(SCr, bCr)
}

# TODO Consider adding aki.ts

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
      "{aki}.bCr" := aki_bCr(!!as.name(SCr), !!as.name(bCr))
    ) %>%
    dplyr::mutate(
      "{aki}.cr_ch" := !!as.name(SCr) * 2
    )
}
