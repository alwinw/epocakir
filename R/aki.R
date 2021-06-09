#' AKI Stages
#'
#' Ordered factor of AKI stages
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @export
#' @examples
#' aki_stages
aki_stages <- factor(
  c("AKI Stage 1", "AKI Stage 2", "AKI Stage 3", "No AKI"),
  levels = c("No AKI", "AKI Stage 1", "AKI Stage 2", "AKI Stage 3"),
  ordered = TRUE
)

#' Codify AKI from Serum Creatinine and/or Urine Output
#'
#' Using KDIGO Clinical Practice Guideline for
#' Acute Kidney Injury
#' Volume 2 | Issue 1 | March 2012
#'
#' Provided a baseline creatinine, series of Serum Creatinine readings and/or
#' Urine Output, `aki_staging()` calculates whether or not a patient has AKI.
#' The staging (1, 2, 3) of AKI is returned.
#'
#' When multiple columns are provided, `aki_staging()` will automatically
#' calculate whether or not AKI has occurred using each KDIGO definition.
#'
#' \itemize{
#'   \item{[`aki_bCr()`]: Staging of AKI based on baseline serum creatinine}
#'   \item{[`aki_SCr()`]: Staging of AKI based on changes in serum creatinine}
#'   \item{[`aki_UO()`]: Staging of AKI based on urine output}
#' }
#'
#' The most severe AKI stage is then returned.
#'
#' See <https://kdigo.org/guidelines/acute-kidney-injury/> for more details.
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCr Serum creatinine
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param bCr Baseline creatinine
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param UO Urine output
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param dttm DateTime
#'   column name, or vector of POSIXct if `.data` is not provided
#' @param pt_id Patient ID
#'   column name, or vector of characters or factors if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return (ordered factor) AKI stages
#' @examples
#' aki_staging(aki_pt_data, SCr = "SCr_", bCr = "bCr_", UO = "UO_", dttm = "dttm_", pt_id = "pt_id_")
#'
#' aki_pt_data %>%
#'   dplyr::mutate(aki = aki_staging(SCr = SCr_, bCr = bCr_, UO = UO_, dttm = dttm_, pt_id = pt_id_))
#' @export
aki_staging <- function(...) {
  ellipsis::check_dots_used()
  UseMethod("aki_staging")
}

#' @rdname aki_staging
#' @export
aki_staging.data.frame <- function(.data,
                                   SCr = NULL,
                                   bCr = NULL,
                                   UO = NULL,
                                   dttm = NULL,
                                   pt_id = NULL,
                                   ...) {
  ellipsis::check_dots_used()
  if (!is.null(SCr)) SCr <- .data[[rlang::as_name(rlang::enquo(SCr))]]
  if (!is.null(bCr)) bCr <- .data[[rlang::as_name(rlang::enquo(bCr))]]
  if (!is.null(UO)) UO <- .data[[rlang::as_name(rlang::enquo(UO))]]
  if (!is.null(dttm)) dttm <- .data[[rlang::as_name(rlang::enquo(dttm))]]
  if (!is.null(pt_id)) pt_id <- .data[[rlang::as_name(rlang::enquo(pt_id))]]

  if (!is.null(SCr)) {
    aki_staging(SCr = SCr, bCr = bCr, UO = UO, dttm = dttm, pt_id = pt_id)
  } else {
    aki_staging(UO = UO, dttm = dttm, pt_id = pt_id)
  }
}

#' @rdname aki_staging
#' @export
aki_staging.units <- function(
                              SCr = NULL,
                              bCr = NULL,
                              UO = NULL,
                              dttm = NULL,
                              pt_id = NULL,
                              ...) {
  ellipsis::check_dots_used()
  if (!is.null(SCr)) SCr <- as_metric(SCr = SCr, value_only = TRUE)
  if (!is.null(bCr)) bCr <- as_metric(SCr = bCr, value_only = TRUE)
  if (!is.null(UO)) UO <- as_metric(UO = UO, value_only = TRUE)

  if (!is.null(SCr)) {
    aki_staging(SCr = SCr, bCr = bCr, UO = UO, dttm = dttm, pt_id = pt_id)
  } else {
    aki_staging(UO = UO, dttm = dttm, pt_id = pt_id)
  }
}

#' @rdname aki_staging
#' @export
aki_staging.numeric <- function(
                                SCr = NULL,
                                bCr = NULL,
                                UO = NULL,
                                dttm = NULL,
                                pt_id = NULL,
                                ...) {
  ellipsis::check_dots_used()

  if (!is.null(dttm) & is.null(pt_id)) {
    warning("Assuming provided data is for a single patient")
    pt_id <- "pt"
  }

  if (!is.null(SCr) & !is.null(bCr)) {
    aki_bCr <- aki_bCr(SCr, bCr)
  } else {
    aki_bCr <- dplyr::last(aki_stages)
  }
  if (!is.null(SCr) & !is.null(dttm)) {
    aki_SCr <- aki_SCr(SCr, dttm, pt_id)
  } else {
    aki_SCr <- dplyr::last(aki_stages)
  }
  if (!is.null(UO) & !is.null(dttm)) {
    aki_UO <- aki_UO(UO, dttm, pt_id)
  } else {
    aki_UO <- dplyr::last(aki_stages)
  }
  return(pmax(aki_bCr, aki_SCr, aki_UO, na.rm = TRUE))
}


#' AKI Staging based on Baseline Serum Creatinine
#'
#' Using KDIGO Clinical Practice Guideline for
#' Acute Kidney Injury
#' Volume 2 | Issue 1 | March 2012
#'
#' \itemize{
#'   \item{AKI Stage 1: 1.5-1.9 times baseline}
#'   \item{AKI Stage 2: 2.0-2.9 times baseline}
#'   \item{AKI Stage 3: 3.0 times baseline}
#' }
#'
#' See <https://kdigo.org/guidelines/acute-kidney-injury/> for more details.
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCr Serum creatinine
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param bCr Baseline creatinine
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return (ordered factor) AKI stages
#' @export
#'
#' @examples
#' aki_bCr(aki_pt_data, SCr = "SCr_", bCr = "bCr_")
#'
#' aki_pt_data %>%
#'   dplyr::mutate(aki = aki_bCr(SCr = SCr_, bCr = bCr_))
aki_bCr <- function(...) {
  UseMethod("aki_bCr")
}

#' @rdname aki_bCr
#' @export
aki_bCr.data.frame <- function(.data, SCr, bCr, ...) {
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
    as_metric(SCr = SCr, value_only = TRUE),
    as_metric(SCr = bCr, value_only = TRUE)
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
    TRUE ~ dplyr::last(aki_stages)
  )
}


#' AKI Staging based on Changes in Serum Creatinine
#'
#' Using KDIGO Clinical Practice Guideline for
#' Acute Kidney Injury
#' Volume 2 | Issue 1 | March 2012
#'
#' \itemize{
#'   \item{AKI Stage 1: \eqn{\ge}0.3 mg/dl (\eqn{\ge}26.5 mmol/l) increase}
#'   \item{AKI Stage 2: N/A}
#'   \item{AKI Stage 3: \eqn{\ge}4.0 mg/dl (\eqn{\ge}353.6 mmol/l)}
#' }
#'
#' See <https://kdigo.org/guidelines/acute-kidney-injury/> for more details.
#'
#' @param .data (data.frame) A data.frame, optional
#' @param dttm DateTime
#'   column name, or vector of POSIXct if `.data` is not provided
#' @param SCr Serum creatinine
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param pt_id Patient ID
#'   column name, or vector of characters or factors if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return (ordered factor) AKI stages
#' @export
#'
#' @examples
#' aki_SCr(aki_pt_data, SCr = "SCr_", dttm = "dttm_", pt_id = "pt_id_")
#'
#' aki_pt_data %>%
#'   dplyr::mutate(aki = aki_SCr(SCr = SCr_, dttm = dttm_, pt_id = pt_id_))
aki_SCr <- function(...) {
  UseMethod("aki_SCr")
}

#' @rdname aki_SCr
#' @export
aki_SCr.data.frame <- function(.data, SCr, dttm, pt_id, ...) {
  ellipsis::check_dots_used()
  aki_SCr(
    .data[[rlang::as_name(rlang::enquo(SCr))]],
    .data[[rlang::as_name(rlang::enquo(dttm))]],
    .data[[rlang::as_name(rlang::enquo(pt_id))]]
  )
}

#' @rdname aki_SCr
#' @export
aki_SCr.units <- function(SCr, dttm, pt_id, ...) {
  ellipsis::check_dots_used()
  aki_SCr(
    as_metric(SCr = SCr, value_only = TRUE),
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
        D.val >= 4.0 ~ aki_stages[3],
        TRUE ~ dplyr::last(aki_stages)
      )
    ) %>%
    dplyr::select(.data$pt_id, .data$dttm, .data$.aki) %>%
    dplyr::group_by(.data$pt_id, .data$dttm) %>%
    dplyr::slice_max(.data$.aki, with_ties = FALSE) %>%
    dplyr::ungroup()

  dplyr::left_join(
    tibble::tibble(pt_id = pt_id, dttm = dttm),
    SCr_changes,
    by = c("pt_id", "dttm")
  ) %>%
    dplyr::mutate(.aki = dplyr::if_else(is.na(.data$.aki), dplyr::last(aki_stages), .data$.aki)) %>%
    dplyr::pull(.data$.aki)
}


#' AKI Staging based on Urine Output
#'
#' Using KDIGO Clinical Practice Guideline for
#' Acute Kidney Injury
#' Volume 2 | Issue 1 | March 2012
#'
#' \itemize{
#'   \item{AKI Stage 1: <0.5 ml/kg/h for 6–12 hours}
#'   \item{AKI Stage 2: <0.5 ml/kg/h for \eqn{\ge}12 hours}
#'   \item{AKI Stage 3: <0.3 ml/kg/h for \eqn{\ge}24 hours OR Anuria for \eqn{\ge}12 hours}
#' }
#'
#' See <https://kdigo.org/guidelines/acute-kidney-injury/> for more details.
#'
#' @param .data (data.frame) A data.frame, optional
#' @param dttm DateTime
#'   column name, or vector of POSIXct if `.data` is not provided
#' @param UO Urine output
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param pt_id Patient ID
#'   column name, or vector of characters or factors if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return (ordered factor) AKI stages
#' @export
#'
#' @examples
#' aki_UO(aki_pt_data, UO = "UO_", dttm = "dttm_", pt_id = "pt_id_")
#'
#' aki_pt_data %>%
#'   dplyr::mutate(aki = aki_UO(UO = UO_, dttm = dttm_, pt_id = pt_id_))
aki_UO <- function(...) {
  UseMethod("aki_UO")
}

#' @rdname aki_UO
#' @export
aki_UO.data.frame <- function(.data, UO, dttm, pt_id, ...) {
  ellipsis::check_dots_used()
  aki_UO(
    .data[[rlang::as_name(rlang::enquo(UO))]],
    .data[[rlang::as_name(rlang::enquo(dttm))]],
    .data[[rlang::as_name(rlang::enquo(pt_id))]]
  )
}

#' @rdname aki_UO
#' @export
aki_UO.units <- function(UO, dttm, pt_id, ...) {
  ellipsis::check_dots_used()
  aki_UO(
    as_metric(UO = UO, value_only = TRUE),
    dttm,
    pt_id
  )
}

#' @rdname aki_UO
#' @export
aki_UO.numeric <- function(UO, dttm, pt_id, ...) {
  ellipsis::check_dots_used()
  UO_df <- data.frame(UO = UO, dttm = dttm, pt_id = pt_id)

  UO_changes <- UO_df %>%
    dplyr::arrange(.data$pt_id, .data$dttm) %>%
    dplyr::group_by(.data$pt_id) %>%
    dplyr::mutate(c_UO = cumsum(.data$UO)) %>%
    combn_changes("dttm", "c_UO", "pt_id") %>%
    dplyr::mutate(
      UOph = .data$D.c_UO / as.numeric(.data$D.dttm, units = "hours"),
      .aki = dplyr::case_when(
        UOph == 0 & D.dttm >= lubridate::duration(hours = 12) ~ aki_stages[3],
        UOph < 0.3 & D.dttm >= lubridate::duration(hours = 24) ~ aki_stages[3],
        UOph < 0.5 & D.dttm >= lubridate::duration(hours = 12) ~ aki_stages[2],
        UOph < 0.5 & D.dttm >= lubridate::duration(hours = 6) ~ aki_stages[1],
        TRUE ~ dplyr::last(aki_stages)
      )
    ) %>%
    dplyr::group_by(.data$pt_id, .data$dttm) %>%
    dplyr::slice_max(.data$.aki, with_ties = FALSE) %>%
    dplyr::ungroup()

  dplyr::left_join(
    UO_df, UO_changes,
    by = c("pt_id", "dttm")
  ) %>%
    dplyr::mutate(.aki = dplyr::if_else(is.na(.data$.aki), dplyr::last(aki_stages), .data$.aki)) %>%
    dplyr::pull(".aki")
}
