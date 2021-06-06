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


#' Conversion Factors
#'
#' List of conversion factors based on tables in  KDIGO Clinical Practice
#' Guidelines.
#'
#' \describe{
#' \item{parameter}{Name of the measurement}
#' \item{metric_units}{Metric units for the parameter}
#' \item{mol_weight}{Molecular weight (where required)}
#' \item{description}{Full name}
#' }
#' @examples
#' epocakir:::conversion_factors
conversion_factors <- tibble::tribble(
  ~parameter, ~metric_units, ~mol_weight, ~description,
  # General
  "Age", "years", NA, "Age",
  "height", "m", NA, "Height",
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
  "Urea", "mg/dl", 60.06, "Urea (plasma)", # changed from AKI 2012 Guideline mg/ml. Correct in CKD Guideline
  # 2012 CKD Guideline
  "SAlb", "g/dl", NA, "Albumin (serum)",
  "Hb", "g/dl", NA, "Hemoglobin",
  # "SPhos", "mg/dl", 94.9714, "Phosphate (serum)",
  # "SPTH", "pg/ml", 3333.9, "Parathyroid Hormone (serum)",
  "UA", "mg/dl", 168.11, "Uric acid",
  # "VitD", "ng/ml", 384.6, "Vitamin D, 25-hydroxyvitamin D"
  # Additional
  "GFR", "mL/min", NA, "Glomerular filtration rate",
  "eGFR", "mL/min/1.73m2", NA, "Estimated glomerular filtration rate",
  "SCysC", "mg/l", NA, "Cystatin C (serum)",
  "AER", "mg/day", NA, "Albumin excretion rate",
  "ACR", "mg/g", NA, "Albumin-to-creatinine ratio",
  "UO", "ml/kg", NA, "Urine Output"
) %>%
  dplyr::mutate(mol_weight = units::set_units(mol_weight, "g/mol"))


#' Convert a measured value to metric units
#'
#' @param param (character) Name of measurement, e.g. param = "SCr"
#' @param meas (units) Measurement or vector of measurements
#' @param ... (units) One of conversion_factors$parameter,
#'   e.g. SCr = units::set_units(88.4, "umol/l").
#'   Case insensitive.
#' @param value_only (logical) Return as value only without units
#'
#' @return (units) Converted measured value or vector of measured values,
#'   unless `value_only = T`
#' @export
#'
#' @examples
#' as_metric(param = "scr", meas = units::set_units(88.4, "umol/l"))
#' as_metric("scr", units::set_units(88.4, "umol/l"))
#'
#' values <- units::set_units(c(60, 70, 80), "umol/l")
#' as_metric(SCr = values)
as_metric <- function(param = NULL, meas = NULL, ..., value_only = FALSE) {
  ellipsis::check_dots_used()
  if (is.null(param) | is.null(meas)) {
    elli <- list(...)
    if (length(elli) == 0) {
      return(NULL)
    } # as_metric(1) will return NULL with no warning
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
    metric_val <- units::set_units(
      meas * conversion$mol_weight,
      conversion$metric_units,
      mode = "standard"
    )
  } else {
    metric_val <- units::set_units(meas, conversion$metric_units, mode = "standard")
  }
  if (value_only) {
    as.double(metric_val)
  } else {
    metric_val
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
#' @return (duration) The age as a duration.
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


#' Convert binary data to factors based on column name
#'
#' @param .data (data.frame) A data frame or data frame extension (e.g. a tibble)
#' @param ... Name-value pairs. The names of columns to be transformed
#'
#' @return (data.frame) An object of the same type as `.data`
#' @export
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, 0, NA, 1, 0),
#'   b = c("y", "n", NA, "Y", "n"),
#'   c = c("yes", "no", NA, "Yes", "No"),
#'   d = c(TRUE, FALSE, NA, TRUE, FALSE),
#'   e = c(1, 2, 3, 4, 5)
#' )
#' binary2factor(df, a, b:d)
#' df %>%
#'   binary2factor(-e)
binary2factor <- function(.data, ...) {
  .data %>% dplyr::mutate(
    dplyr::across(
      c(...),
      function(x) {
        b <- dplyr::case_when(
          tolower(x) %in% c("y", "1", "yes", "true") ~ 1,
          tolower(x) %in% c("n", "0", "no", "false") ~ 0,
          is.na(x) ~ NA_real_,
          TRUE ~ NaN
        )
        factor(b, c(0, 1), paste0(c("Not_", ""), dplyr::cur_column()), ordered = TRUE)
      }
    )
  )
}

# Internal helper functions for combine_date_time_cols
set_names <- function(.data, names) {
  names(.data) <- names
  .data
}

find_cols <- function(text, replace, colnames) {
  data.frame(
    i = grep(paste0("^", text, "|", text, "$"), colnames, ignore.case = TRUE),
    j = grep(paste0("^", text, "|", text, "$"), colnames, ignore.case = TRUE, value = TRUE),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(k = gsub(text, replace, .data$j, ignore.case = TRUE)) %>%
    set_names(c(paste0(text, "_i"), paste0(text), "match"))
}


#' Combine date and time columns into a single DateTime column
#'
#' @param .data (data.frame) A data frame or data frame extension (e.g. a tibble)
#' @param tz (character) a time zone name (default: time zone of the POSIXt
#' object x)
#'
#' @return (data.frame) An object of the same type as `.data`
#' @export
#'
#' @examples
#' df <- data.frame(
#'   date_a = as.Date(c("2020-01-01", "2020-01-02")),
#'   date_b = as.POSIXct(c("2020-02-01", "2020-02-02")),
#'   time_a = as.POSIXct(c("1900-01-01 01:01:01", "1900-01-01 02:02:02")),
#'   time_b = as.POSIXct(c("1900-01-01 01:01:01", "1900-01-01 02:02:02"))
#' )
#'
#' combine_date_time_cols(df)
combine_date_time_cols <- function(.data, tz = NULL) {
  dttm_col <- dplyr::inner_join(
    find_cols("date", "DateTime", colnames(.data)),
    find_cols("time", "DateTime", colnames(.data)),
    by = "match"
  ) %>%
    dplyr::select(.data$date, .data$time, .data$match) %>%
    tidyr::pivot_longer(-.data$match, values_to = "raw") %>%
    dplyr::select(-.data$name)

  new_col_names <- dplyr::left_join(
    data.frame(raw = colnames(.data)), dttm_col,
    by = "raw"
  ) %>%
    dplyr::mutate(match = dplyr::if_else(is.na(match), raw, match)) %>%
    dplyr::pull(match) %>%
    unique(.data)

  .data %>%
    tibble::rownames_to_column(var = "_rowname") %>%
    tidyr::pivot_longer(
      dplyr::all_of(dttm_col$raw),
      names_to = "DateTimeName",
      values_to = "DateTime"
    ) %>%
    dplyr::mutate(
      DateTimeType = dplyr::if_else(grepl("^time|time$", .data$DateTimeName, ignore.case = TRUE), "Time", ""),
      DateTimeType = dplyr::if_else(grepl("^date|date$", .data$DateTimeName, ignore.case = TRUE), "Date", .data$DateTimeType),
      DateTimeName = gsub("^time|time$|^date|date$", "DateTime", .data$DateTimeName, ignore.case = TRUE)
    ) %>%
    tidyr::pivot_wider(
      names_from = "DateTimeType",
      values_from = "DateTime"
    ) %>%
    dplyr::mutate(
      datetime = dplyr::if_else(
        (is.na(.data$Date) | is.na(.data$Time)),
        NA_character_,
        paste(format(.data$Date, format = "%Y-%m-%d"), format(.data$Time, format = "%H:%M:%S"))
      ),
      Date = NULL,
      Time = NULL
    ) %>%
    dplyr::mutate(datetime = lubridate::as_datetime(.data$datetime, tz = tz)) %>%
    tidyr::pivot_wider(
      names_from = "DateTimeName",
      values_from = "datetime"
    ) %>%
    dplyr::select(dplyr::all_of(new_col_names))
}


#' Combinatorics changes
#'
#' Compares a value with all previous values
#'
#' @param .data (data.frame) A data.frame, optional
#' @param dttm DateTime
#'   column name, or vector of POSIXct if `.data` is not provided
#' @param val Variable
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param pt_id Patient ID
#'   column name, or vector of characters or factors if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return Combinatorics changes
#'   of the same type provided (numeric or units)
#' @export
#'
#' @examples
#' combn_changes(aki_pt_data, dttm = "dttm_", val = "SCr_", pt_id = "pt_id_")
#'
#' aki_pt_data %>%
#'   combn_changes(dttm_, SCr_, pt_id_)
combn_changes <- function(...) {
  UseMethod("combn_changes")
}

#' @rdname combn_changes
#' @export
combn_changes.data.frame <- function(.data, dttm, val, pt_id, ...) {
  ellipsis::check_dots_used()
  val_name <- rlang::as_name(rlang::enquo(val))
  dttm_name <- rlang::as_name(rlang::enquo(dttm))
  pt_id_name <- rlang::as_name(rlang::enquo(pt_id))
  data_n <- combn_changes(
    .data[[dttm_name]],
    .data[[val_name]],
    .data[[pt_id_name]]
  )
  colnames(data_n) <- c(
    pt_id_name, dttm_name, val_name,
    paste0("D.", val_name), paste0("D.", dttm_name)
  )
  return(data_n)
}

#' @rdname combn_changes
#' @export
combn_changes.POSIXct <- function(dttm, val, pt_id, ...) {
  data_gr <- tibble::tibble(
    dttm = dttm,
    val = val,
    pt_id = pt_id
  ) %>%
    dplyr::group_by(.data$pt_id, .add = FALSE) %>%
    dplyr::arrange(.data$pt_id, .data$dttm) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      admin = cumsum(
        (dttm - dplyr::lag(dttm, default = lubridate::as_date(0))) >=
          lubridate::duration(hours = 48)
      )
    ) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$admin, .add = TRUE)

  data_n <- data_gr %>%
    dplyr::count() %>%
    dplyr::filter(.data$n > 1) %>% # prevent n < m error in combn
    dplyr::ungroup() %>%
    dplyr::mutate(n_1 = cumsum(dplyr::lag(.data$n, default = 0))) %>%
    dplyr::rowwise() %>%
    dplyr::do(data.frame(.data$n_1 + t(utils::combn(.data$n, 2)))) %>%
    dplyr::arrange(.data$X2, dplyr::desc(.data$X1))
  # [ ]: consider a more dplyr version e.g. pivot_longer (X1, X2) then use summarise and diff
  T1 <- data_gr[data_n$X1, ]
  T2 <- data_gr[data_n$X2, ]
  # The patient id should also match, remove in future if warning never raised
  if (!all.equal(T1[c("pt_id", "admin")], T2[c("pt_id", "admin")])) {
    warning("Unexpected mismatch in patient ids") # nocov
  }
  tibble::tibble(
    pt_id = T1$pt_id,
    admin = T1$admin,
    dttm = T2$dttm,
    val = T2$val,
    D.val = T2$val - T1$val,
    D.dttm = T2$dttm - T1$dttm
  ) %>%
    dplyr::filter(.data$D.dttm <= lubridate::duration(hours = 48)) %>%
    dplyr::select(.data$pt_id, .data$dttm:.data$D.dttm)
}
