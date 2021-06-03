
aki_stages <- factor(c("AKI Stage 1", "AKI Stage 2", "AKI Stage 3", "No AKI"),
  levels = c("No AKI", "AKI Stage 1", "AKI Stage 2", "AKI Stage 3"),
  ordered = TRUE
)

aki_staging <- function() {}

#' Codify AKI from Serum Creatinine and/or Urine Output
#'
#' Using KDIGO Clinical Practice Guideline for Acute Kidney Injury
#' Volume 2 | Issue 1 | March 2012
#'
#' Provided a series of Serum Creatinine readings and/or Urine Output, aki()
#' calculates whether or not a patient has AKI. The staging (1, 2, 3) of AKI is
#' also calculated
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCr Serum creatinine
#'   column name, or vector if `.data` not provided
#' @param bCr Baseline creatinine
#'   column name, or vector if `.data` not provided
#' @param UO Urine output
#'   column name, or vector if `.data` not provided
#' @param dttm DateTime
#'   column name, or vector if `.data` not provided
#' @param pt_id Patient ID
#'   column name, or vector if `.data` not provided
#' @param ... Further optional arguments
#'
#' @examples
#' print("todo")
#' @importFrom rlang .data
#' @importFrom rlang `:=`
#' @export
aki <- function(...) {
  ellipsis::check_dots_used()
  UseMethod("aki")
}

#' @rdname aki
#' @export
aki.default <- function(.data,
                        SCr = NULL,
                        bCr = NULL,
                        UO = NULL,
                        dttm = NULL,
                        pt_id = NULL,
                        ...) {
  ellipsis::check_dots_used()
}

#' @rdname aki
#' @export
aki.units <- function(
                      SCr = NULL,
                      bCr = NULL,
                      UO = NULL,
                      dttm = NULL,
                      pt_id = NULL,
                      ...) {
  ellipsis::check_dots_used()
  if (!is.null(SCr)) SCr <- as_metric(SCr = SCr, value_only = T)
  if (!is.null(bCr)) bCr <- as_metric(SCr = bCr, value_only = T)
  if (!is.null(UO)) UO <- as_metric(UO = UO, value_only = T)

  aki(SCr = SCr, bCr = bCr, UO = UO, dttm = dttm, pt_id = pt_id)
}

#' @rdname aki
#' @export
aki.numeric <- function(
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
    aki_bCr <- aki_bCr(SCr)
  } else {
    aki_bCr <- dplyr::last(aki_stages)
  }
  if (!is.null(SCr) & !is.null(dttm)) {
    aki_SCr <- aki_SCr(SCr, dttm, pt_id)
  }

  # FIXME: Consider re-writting aki_stages to include no AKI
  # Will make pmax and other methods easier

  aki <- pmax(aki_bCr, aki_SCr, na.rm = TRUE)

  return(NULL)
}


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
    TRUE ~ dplyr::last(aki_stages)
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
    dplyr::select(.data$pt_id, .data$dttm, .data$.aki) %>%
    dplyr::group_by(.data$pt_id, .data$dttm) %>%
    dplyr::slice_max(.data$.aki, with_ties = FALSE) %>%
    dplyr::ungroup()

  dplyr::left_join(
    tibble::tibble(pt_id = pt_id, dttm = dttm),
    SCr_changes,
    by = c("pt_id", "dttm")
  ) %>%
    dplyr::pull(.data$.aki)
}

#' AKI Staging based on urine output
#'
#' @param .data (data.frame) A data.frame, optional
#' @param dttm DateTime
#'   column name, or vector if `.data` not provided
#' @param UO Urine output
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
aki_UO <- function(...) {
  UseMethod("aki_UO")
}

#' @rdname aki_UO
#' @export
aki_UO.default <- function(.data, UO, dttm, pt_id, ...) {
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
    as_metric(UO = UO, value_only = T),
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
