#' GFR Estimation
#'
#' Using KDIGO 2012 Clinical Practice Guideline for
#' the Evaluation and Management of Chronic Kidney Disease
#' Volume 3 | Issue 1 | January 2013
#'
#' Automatic selection of equation to estimation the
#' Glomerular Filtration Rate (eGFR), based on input data
#'
#' \itemize{
#'   \item{[`eGFR_adult_SCr()`]: 2009 CKD-EPI creatinine equation}
#'   \item{[`eGFR_adult_SCysC()`]: 2012 CKD-EPI cystatin C equation}
#'   \item{[`eGFR_adult_SCr_SCysC()`]: 2012 CKD-EPI creatinine-cystatin C equation}
#'   \item{[`eGFR_child_SCr()`]: Pediatric creatinine-based equation}
#'   \item{[`eGFR_child_SCr_BUN()`]: Pediatric creatinine-BUN equation}
#'   \item{[`eGFR_child_SCysC()`]: Pediatric cystatin C-based equation}
#' }
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCr Serum creatinine
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param SCysC Serum Cystatin C
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param Age Age of patient
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param height Height of patient
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param BUN Blood urea nitrogen
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param male Male or not
#'   column name, or vector of logical (TRUE/FALSE) if `.data` is not provided
#' @param black Black race or not
#'   column name, or vector of logical (TRUE/FALSE) if `.data` is not provided
#' @param pediatric (logical) Pediatric or not
#'   column name, or vector of logical (TRUE/FALSE) if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return (units) Estimated glomerular filtration rate (eGFR)
#'   of the same type provided (numeric or units in ml/min/1.73m2)
#' @export
#'
#' @examples
#' eGFR(eGFR_pt_data,
#'   SCr = "SCr_", SCysC = "SCysC_",
#'   Age = "Age_", height = "height_", BUN = "BUN_",
#'   male = "male_", black = "black_", pediatric = "pediatric_"
#' )
#'
#' eGFR_pt_data %>%
#'   dplyr::mutate(eGFR = eGFR(
#'     SCr = SCr_, SCysC = SCysC_,
#'     Age = Age_, height = height_, BUN = BUN_,
#'     male = male_, black = black_, pediatric = pediatric_
#'   ))
eGFR <- function(...) {
  UseMethod("eGFR")
}

eGFR_internal <- function(SCr,
                          SCysC,
                          Age,
                          height,
                          BUN,
                          male,
                          black,
                          pediatric) {
  if (!is.na(SCr) & is.na(SCysC) & !is.na(Age) & !is.na(male) & !is.na(black) & !pediatric) {
    eGFR_adult_SCr(SCr, Age, male, black)
  } else if (is.na(SCr) & !is.na(SCysC) & !is.na(Age) & !is.na(male) & !pediatric) {
    eGFR_adult_SCysC(SCysC, Age, male)
  } else if (!is.na(SCr) & !is.na(SCysC) & !is.na(Age) & !is.na(male) & !is.na(black) & !pediatric) {
    eGFR_adult_SCr_SCysC(SCr, SCysC, Age, male, black)
  } else if (!is.na(SCr) & !is.na(height) & is.na(BUN) & pediatric) {
    eGFR_child_SCr(SCr, height)
  } else if (!is.na(SCr) & !is.na(height) & !is.na(BUN) & pediatric) {
    eGFR_child_SCr_BUN(SCr, height, BUN)
  } else if (!is.na(SCysC) & pediatric) {
    eGFR_child_SCysC(SCysC)
  } else {
    warning("Could not find an appropriate eGFR() formula to use")
    NA_real_
  }
}

#' @rdname eGFR
#' @export
eGFR.data.frame <- function(.data,
                            SCr = NULL,
                            SCysC = NULL,
                            Age = NULL,
                            height = NULL,
                            BUN = NULL,
                            male = NULL,
                            black = NULL,
                            pediatric = NULL,
                            ...) {
  ellipsis::check_dots_used()
  # [ ] !is.null won't work if SCr is a symbol...
  if (!is.null(SCr)) SCr <- .data[[rlang::as_name(rlang::enquo(SCr))]]
  if (!is.null(SCysC)) SCysC <- .data[[rlang::as_name(rlang::enquo(SCysC))]]
  if (!is.null(Age)) Age <- .data[[rlang::as_name(rlang::enquo(Age))]]
  if (!is.null(height)) height <- .data[[rlang::as_name(rlang::enquo(height))]]
  if (!is.null(BUN)) BUN <- .data[[rlang::as_name(rlang::enquo(BUN))]]
  if (!is.null(male)) male <- .data[[rlang::as_name(rlang::enquo(male))]]
  if (!is.null(black)) black <- .data[[rlang::as_name(rlang::enquo(black))]]
  if (!is.null(pediatric)) pediatric <- .data[[rlang::as_name(rlang::enquo(pediatric))]]

  if (!is.null(SCr)) {
    eGFR(
      SCr = SCr, SCysC = SCysC,
      Age = Age, height = height, BUN = BUN,
      male = male, black = black, pediatric = pediatric
    )
  } else if (!is.null(SCysC)) {
    eGFR(
      SCysC = SCysC,
      Age = Age, height = height, BUN = BUN,
      male = male, black = black, pediatric = pediatric
    )
  }
}

#' @rdname eGFR
#' @export
eGFR.units <- function(SCr = NULL,
                       SCysC = NULL,
                       Age = NULL,
                       height = NULL,
                       BUN = NULL,
                       male = NULL,
                       black = NULL,
                       pediatric = NULL,
                       ...) {
  ellipsis::check_dots_used()
  if (!is.null(SCr)) SCr <- as_metric(SCr = SCr, value_only = TRUE)
  if (!is.null(SCysC)) SCysC <- as_metric(SCysC = SCysC, value_only = TRUE)
  if (!is.null(Age)) Age <- as_metric(Age = Age, value_only = TRUE)
  if (!is.null(height)) height <- as_metric(height = height, value_only = TRUE)
  if (!is.null(BUN)) BUN <- as_metric(BUN = BUN, value_only = TRUE)

  if (!is.null(SCr)) {
    eGFR_calc <- eGFR(
      SCr = SCr, SCysC = SCysC,
      Age = Age, height = height, BUN = BUN,
      male = male, black = black, pediatric = pediatric
    )
  } else if (!is.null(SCysC)) {
    eGFR_calc <- eGFR(
      SCysC = SCysC,
      Age = Age, height = height, BUN = BUN,
      male = male, black = black, pediatric = pediatric
    )
  }

  units::set_units(eGFR_calc, "mL/min/1.73m2")
}

#' @rdname eGFR
#' @export
eGFR.numeric <- function(SCr = NULL,
                         SCysC = NULL,
                         Age = NULL,
                         height = NULL,
                         BUN = NULL,
                         male = NULL,
                         black = NULL,
                         pediatric = NULL,
                         ...) {
  ellipsis::check_dots_used()
  cols <- c(SCr = NA, SCysC = NA, Age = NA, height = NA, BUN = NA, male = NA, black = NA, pediatric = NA)
  df <- cbind(SCr, SCysC, Age, height, BUN, male, black, pediatric) %>%
    tibble::as_tibble(.data)
  df <- tibble::add_column(df, !!!cols[!names(cols) %in% names(df)]) %>%
    dplyr::mutate(dplyr::across(c(male, black, pediatric), as.logical))
  # Unsure why add_column can't be %>% chained in with names(.data)

  if (is.null(Age) & is.null(pediatric)) {
    warning("Either Age or pediatric should be provided. Assuming pediatric patients as Age must be provided for adults.")
    df <- dplyr::mutate(df, pediatric = TRUE)
  } else if (!is.null(Age) & is.null(pediatric)) {
    df <- dplyr::mutate(df, pediatric = Age < 18)
  } else if (is.null(Age) & !is.null(pediatric)) {
    df <- dplyr::mutate(df, pediatric = !!pediatric)
  } else {
    check_ped_ok <- all.equal(df$Age < 18, df$pediatric)
    if (is.character(check_ped_ok)) {
      stop(paste("Inconsistencies found between pediatric and age colums:", check_ped_ok))
    }
    df <- dplyr::mutate(df, pediatric = !!pediatric)
  }

  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(eGFR = eGFR_internal(SCr, SCysC, Age, height, BUN, male, black, pediatric)) %>%
    dplyr::pull(eGFR)
}


#' eGFR 2009 CKD-EPI creatinine equation
#'
#' Using KDIGO 2012 Clinical Practice Guideline for
#' the Evaluation and Management of Chronic Kidney Disease
#' Volume 3 | Issue 1 | January 2013
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCr Serum creatinine
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param Age Age of patient
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param male Male or not
#'   column name, or vector of logical (TRUE/FALSE) if `.data` is not provided
#' @param black Black race or not
#'   column name, or vector of logical (TRUE/FALSE) if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return Estimated GFR
#'   of the same type provided (numeric or units)
#' @export
#'
#' @examples
#' eGFR_adult_SCr(eGFR_pt_data,
#'   SCr = "SCr_", Age = "Age_", male = "male_", black = "black_"
#' )
#'
#' eGFR_pt_data %>%
#'   dplyr::mutate(eGFR = eGFR_adult_SCr(
#'     SCr = SCr_, Age = Age_, male = male_, black = black_
#'   ))
eGFR_adult_SCr <- function(...) {
  UseMethod("eGFR_adult_SCr")
}

#' @rdname eGFR_adult_SCr
#' @export
eGFR_adult_SCr.data.frame <- function(.data, SCr, Age, male, black, ...) {
  ellipsis::check_dots_used()
  eGFR_adult_SCr(
    .data[[rlang::as_name(rlang::enquo(SCr))]],
    .data[[rlang::as_name(rlang::enquo(Age))]],
    .data[[rlang::as_name(rlang::enquo(male))]],
    .data[[rlang::as_name(rlang::enquo(black))]],
  )
}

#' @rdname eGFR_adult_SCr
#' @export
eGFR_adult_SCr.units <- function(SCr, Age, male, black, ...) {
  ellipsis::check_dots_used()
  eGFR <- eGFR_adult_SCr(
    as_metric(SCr = SCr, value_only = TRUE),
    as_metric(Age = Age, value_only = TRUE),
    male,
    black
  )
  units::set_units(eGFR, "mL/min/1.73m2")
}

#' @rdname eGFR_adult_SCr
#' @export
eGFR_adult_SCr.numeric <- function(SCr, Age, male, black, ...) {
  ellipsis::check_dots_used()
  male <- as.logical(male)
  black <- as.logical(black)
  kappa <- dplyr::if_else(!male, 0.7, 0.9)
  alpha <- dplyr::if_else(!male, -0.329, -0.411)
  141 * pmin(SCr / kappa, 1)^alpha * pmax(SCr / kappa, 1)^-1.209 * 0.993^Age *
    dplyr::if_else(male, 1, 1.018) *
    dplyr::if_else(black, 1.159, 1)
}


#' eGFR 2012 CKD-EPI cystatin C equation
#'
#' Using KDIGO 2012 Clinical Practice Guideline for
#' the Evaluation and Management of Chronic Kidney Disease
#' Volume 3 | Issue 1 | January 2013
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCysC Serum Cystatin C
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param Age Age of patient
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param male Male or not
#'   column name, or vector of logical (TRUE/FALSE) if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return Estimated GFR
#'   of the same type provided (numeric or units)
#' @export
#'
#' @examples
#' eGFR_adult_SCysC(eGFR_pt_data,
#'   SCysC = "SCysC_", Age = "Age_", male = "male_"
#' )
#'
#' eGFR_pt_data %>%
#'   dplyr::mutate(eGFR = eGFR_adult_SCysC(
#'     SCysC = SCysC_, Age = Age_, male = male_
#'   ))
eGFR_adult_SCysC <- function(...) {
  UseMethod("eGFR_adult_SCysC")
}

#' @rdname eGFR_adult_SCysC
#' @export
eGFR_adult_SCysC.data.frame <- function(.data, SCysC, Age, male, ...) {
  ellipsis::check_dots_used()
  eGFR_adult_SCysC(
    .data[[rlang::as_name(rlang::enquo(SCysC))]],
    .data[[rlang::as_name(rlang::enquo(Age))]],
    .data[[rlang::as_name(rlang::enquo(male))]]
  )
}

#' @rdname eGFR_adult_SCysC
#' @export
eGFR_adult_SCysC.units <- function(SCysC, Age, male, ...) {
  ellipsis::check_dots_used()
  eGFR <- eGFR_adult_SCysC(
    as_metric(SCysC = SCysC, value_only = TRUE),
    as_metric(Age = Age, value_only = TRUE),
    male
  )
  units::set_units(eGFR, "mL/min/1.73m2")
}

#' @rdname eGFR_adult_SCysC
#' @export
eGFR_adult_SCysC.numeric <- function(SCysC, Age, male, ...) {
  ellipsis::check_dots_used()
  male <- as.logical(male)
  133 * pmin(SCysC / 0.8, 1)^-0.499 * pmax(SCysC / 0.8, 1)^-1.328 * 0.996^Age *
    dplyr::if_else(male, 1, 0.932)
}


#' eGFR 2012 CKD-EPI creatinine-cystatin C equation
#'
#' Using KDIGO 2012 Clinical Practice Guideline for
#' the Evaluation and Management of Chronic Kidney Disease
#' Volume 3 | Issue 1 | January 2013
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCr Serum creatinine
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param SCysC Serum Cystatin C
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param Age Age of patient
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param male Male or not
#'   column name, or vector of logical (TRUE/FALSE) if `.data` is not provided
#' @param black Black race or not
#'   column name, or vector of logical (TRUE/FALSE) if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return Estimated GFR
#'   of the same type provided (numeric or units)
#' @export
#'
#' @examples
#' eGFR_adult_SCr_SCysC(eGFR_pt_data,
#'   SCr = "SCr_", SCysC = "SCysC_",
#'   Age = "Age_", male = "male_", black = "black_"
#' )
#'
#' eGFR_pt_data %>%
#'   dplyr::mutate(eGFR = eGFR_adult_SCr_SCysC(
#'     SCr = SCr_, SCysC = SCysC_,
#'     Age = Age_, male = male_, black = black_
#'   ))
eGFR_adult_SCr_SCysC <- function(...) {
  UseMethod("eGFR_adult_SCr_SCysC")
}

#' @rdname eGFR_adult_SCr_SCysC
#' @export
eGFR_adult_SCr_SCysC.data.frame <- function(.data, SCr, SCysC, Age, male, black, ...) {
  ellipsis::check_dots_used()
  eGFR_adult_SCr_SCysC(
    .data[[rlang::as_name(rlang::enquo(SCr))]],
    .data[[rlang::as_name(rlang::enquo(SCysC))]],
    .data[[rlang::as_name(rlang::enquo(Age))]],
    .data[[rlang::as_name(rlang::enquo(male))]],
    .data[[rlang::as_name(rlang::enquo(black))]]
  )
}

#' @rdname eGFR_adult_SCr_SCysC
#' @export
eGFR_adult_SCr_SCysC.units <- function(SCr, SCysC, Age, male, black, ...) {
  ellipsis::check_dots_used()
  eGFR <- eGFR_adult_SCr_SCysC(
    as_metric(SCr = SCr, value_only = TRUE),
    as_metric(SCysC = SCysC, value_only = TRUE),
    as_metric(Age = Age, value_only = TRUE),
    male,
    black
  )
  units::set_units(eGFR, "mL/min/1.73m2")
}

#' @rdname eGFR_adult_SCr_SCysC
#' @export
eGFR_adult_SCr_SCysC.numeric <- function(SCr, SCysC, Age, male, black, ...) {
  ellipsis::check_dots_used()
  male <- as.logical(male)
  black <- as.logical(black)
  kappa <- dplyr::if_else(!male, 0.7, 0.9)
  alpha <- dplyr::if_else(!male, -0.248, -0.207)
  eGFR <- 135 * pmin(SCr / kappa, 1)^alpha * pmax(SCr / kappa, 1)^-0.601 *
    pmin(SCysC / 0.8, 1)^-0.375 * pmax(SCysC / 0.8, 1)^-0.711 *
    0.995^Age *
    dplyr::if_else(male, 1, 0.969) *
    dplyr::if_else(black, 1.08, 1)
}


#' eGFR Pediatric SCr and Height
#'
#' Using KDIGO 2012 Clinical Practice Guideline for
#' the Evaluation and Management of Chronic Kidney Disease
#' Volume 3 | Issue 1 | January 2013
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCr Serum creatinine
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param height Height of patient
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return Estimated GFR
#'   of the same type provided (numeric or units)
#' @export
#'
#' @examples
#' eGFR_child_SCr(eGFR_pt_data,
#'   SCr = "SCr_", height = "height_"
#' )
#'
#' eGFR_pt_data %>%
#'   dplyr::mutate(eGFR = eGFR_child_SCr(
#'     SCr = SCr_, height = height_,
#'   ))
eGFR_child_SCr <- function(...) {
  UseMethod("eGFR_child_SCr")
}

#' @rdname eGFR_child_SCr
#' @export
eGFR_child_SCr.data.frame <- function(.data, SCr, height, ...) {
  ellipsis::check_dots_used()
  eGFR_child_SCr(
    .data[[rlang::as_name(rlang::enquo(SCr))]],
    .data[[rlang::as_name(rlang::enquo(height))]]
  )
}

#' @rdname eGFR_child_SCr
#' @export
eGFR_child_SCr.units <- function(SCr, height, ...) {
  ellipsis::check_dots_used()
  eGFR <- eGFR_child_SCr(
    as_metric(SCr = SCr, value_only = TRUE),
    as_metric(height = height, value_only = TRUE),
  )
  units::set_units(eGFR, "mL/min/1.73m2")
}

#' @rdname eGFR_child_SCr
#' @export
eGFR_child_SCr.numeric <- function(SCr, height, ...) {
  ellipsis::check_dots_used()
  41.3 * (height / SCr)
}


#' eGFR Pediatric SCr, Height and BUN
#'
#' Using KDIGO 2012 Clinical Practice Guideline for
#' the Evaluation and Management of Chronic Kidney Disease
#' Volume 3 | Issue 1 | January 2013
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCr Serum creatinine
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param height Height of patient
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param BUN Blood urea nitrogen
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return Estimated GFR
#'   of the same type provided (numeric or units)
#' @export
#'
#' @examples
#' eGFR_child_SCr_BUN(eGFR_pt_data,
#'   SCr = "SCr_", height = "height_", BUN = "BUN_",
#' )
#'
#' eGFR_pt_data %>%
#'   dplyr::mutate(eGFR = eGFR_child_SCr_BUN(
#'     SCr = SCr_, height = height_, BUN = BUN_,
#'   ))
eGFR_child_SCr_BUN <- function(...) {
  UseMethod("eGFR_child_SCr_BUN")
}

#' @rdname eGFR_child_SCr_BUN
#' @export
eGFR_child_SCr_BUN.data.frame <- function(.data, SCr, height, BUN, ...) {
  ellipsis::check_dots_used()
  eGFR_child_SCr_BUN(
    .data[[rlang::as_name(rlang::enquo(SCr))]],
    .data[[rlang::as_name(rlang::enquo(height))]],
    .data[[rlang::as_name(rlang::enquo(BUN))]]
  )
}

#' @rdname eGFR_child_SCr_BUN
#' @export
eGFR_child_SCr_BUN.units <- function(SCr, height, BUN, ...) {
  ellipsis::check_dots_used()
  eGFR <- eGFR_child_SCr_BUN(
    as_metric(SCr = SCr, value_only = TRUE),
    as_metric(height = height, value_only = TRUE),
    as_metric(BUN = BUN, value_only = TRUE)
  )
  units::set_units(eGFR, "mL/min/1.73m2")
}

#' @rdname eGFR_child_SCr_BUN
#' @export
eGFR_child_SCr_BUN.numeric <- function(SCr, height, BUN, ...) {
  ellipsis::check_dots_used()
  40.7 * (height / SCr)^0.64 * (30 / BUN)^0.202
}


#' eGFR Pediatric SCysC
#'
#' Using KDIGO 2012 Clinical Practice Guideline for
#' the Evaluation and Management of Chronic Kidney Disease
#' Volume 3 | Issue 1 | January 2013
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @param .data (data.frame) A data.frame, optional
#' @param SCysC Serum Cystatin C
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return Estimated GFR
#'   of the same type provided (numeric or units)
#' @export
#'
#' @examples
#' eGFR_child_SCysC(eGFR_pt_data,
#'   SCysC = "SCysC_"
#' )
#'
#' eGFR_pt_data %>%
#'   dplyr::mutate(eGFR = eGFR_child_SCysC(
#'     SCysC = SCysC_
#'   ))
eGFR_child_SCysC <- function(...) {
  UseMethod("eGFR_child_SCysC")
}

#' @rdname eGFR_child_SCysC
#' @export
eGFR_child_SCysC.data.frame <- function(.data, SCysC, ...) {
  ellipsis::check_dots_used()
  eGFR_child_SCysC(
    .data[[rlang::as_name(rlang::enquo(SCysC))]]
  )
}

#' @rdname eGFR_child_SCysC
#' @export
eGFR_child_SCysC.units <- function(SCysC, ...) {
  ellipsis::check_dots_used()
  eGFR <- eGFR_child_SCysC(
    as_metric(SCysC = SCysC, value_only = TRUE)
  )
  units::set_units(eGFR, "mL/min/1.73m2")
}

#' @rdname eGFR_child_SCysC
#' @export
eGFR_child_SCysC.numeric <- function(SCysC, ...) {
  ellipsis::check_dots_used()
  70.69 * (SCysC)^-0.931
}


#' GFR Stages
#'
#' Ordered factor of GFR stages
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @export
#' @examples
#' GFR_stages
GFR_stages <- factor(c("G1", "G2", "G3a", "G3b", "G4", "G5"), ordered = TRUE)


#' GFR Staging
#'
#' Using KDIGO 2012 Clinical Practice Guideline for
#' the Evaluation and Management of Chronic Kidney Disease
#' Volume 3 | Issue 1 | January 2013
#'
#' \itemize{
#'   \item{G1: Normal or high GFR, \eqn{\ge}90}
#'   \item{G2: Mildly decreased, 60-89}
#'   \item{G3a: Mildly to moderately decreased, 45-59}
#'   \item{G3b: Moderately  to severely decreased, 30-44}
#'   \item{G4: Severely decreased, 15-29}
#'   \item{G5: Kidney failure, <15}
#' }
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @param .data (data.frame) A data.frame, optional
#' @param GFR Glomerular filtration rate
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return GFR category
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   eGFR = units::set_units(c(-1, NA, 100, 70, 50, 35, 20, 10), "mL/min/1.73m2")
#' )
#'
#' GFR_staging(df, "eGFR")
#'
#' df %>%
#'   dplyr::mutate(GFR_level = GFR_staging(eGFR))
GFR_staging <- function(...) {
  UseMethod("GFR_staging")
}

#' @rdname GFR_staging
#' @export
GFR_staging.data.frame <- function(.data, GFR, ...) {
  GFR_staging(
    .data[[rlang::as_name(rlang::enquo(GFR))]]
  )
}

#' @rdname GFR_staging
#' @export
GFR_staging.units <- function(GFR, ...) {
  if (grepl("1.73m2-1", units::deparse_unit(GFR))) {
    GFR <- GFR * units::set_units(1, "1.73m2")
  }
  GFR_staging(
    as_metric(GFR = GFR, value_only = TRUE)
  )
}

#' @rdname GFR_staging
#' @export
GFR_staging.numeric <- function(GFR, ...) {
  dplyr::case_when(
    GFR >= 90 ~ GFR_stages[1],
    GFR >= 60 ~ GFR_stages[2],
    GFR >= 45 ~ GFR_stages[3],
    GFR >= 30 ~ GFR_stages[4],
    GFR >= 15 ~ GFR_stages[5],
    GFR >= 0 ~ GFR_stages[6],
    TRUE ~ GFR_stages[NA_integer_] # GFR should be positive
  )
}


#' Albuminuria Stages
#'
#' Ordered factor of Albuminuria stages
#'
#' \itemize{
#'   \item{A1: Normal to mildly increased}
#'   \item{A2: Moderately increased}
#'   \item{A3: Severely increased}
#' }
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @export
#' @examples
#' Albuminuria_stages
Albuminuria_stages <- factor(
  c("A1", "A2", "A3", "No Albuminuria"),
  levels = c("No Albuminuria", "A1", "A2", "A3"),
  ordered = TRUE
)


#' Albuminuria Staging based on AER
#'
#' Using KDIGO 2012 Clinical Practice Guideline for
#' the Evaluation and Management of Chronic Kidney Disease
#' Volume 3 | Issue 1 | January 2013
#'
#' \itemize{
#'   \item{A1: Normal to mildly increased}
#'   \item{A2: Moderately increased}
#'   \item{A3: Severely increased}
#' }
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @param .data (data.frame) A data.frame, optional
#' @param AER Albumin excretion rate
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return Albuminuria category
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   AER = units::set_units(c(-1, NA, 15, 100, 500), "mg/day")
#' )
#'
#' Albuminuria_staging_AER(df, "AER")
#'
#' df %>%
#'   dplyr::mutate(GFR_level = Albuminuria_staging_AER(AER))
Albuminuria_staging_AER <- function(...) {
  UseMethod("Albuminuria_staging_AER")
}

#' @rdname Albuminuria_staging_AER
#' @export
Albuminuria_staging_AER.data.frame <- function(.data, AER, ...) {
  Albuminuria_staging_AER(
    .data[[rlang::as_name(rlang::enquo(AER))]]
  )
}

#' @rdname Albuminuria_staging_AER
#' @export
Albuminuria_staging_AER.units <- function(AER, ...) {
  Albuminuria_staging_AER(
    as_metric(AER = AER, value_only = TRUE)
  )
}

#' @rdname Albuminuria_staging_AER
#' @export
Albuminuria_staging_AER.numeric <- function(AER, ...) {
  dplyr::case_when(
    AER > 300 ~ Albuminuria_stages[3],
    AER > 30 ~ Albuminuria_stages[2],
    AER > 0 ~ Albuminuria_stages[1],
    TRUE ~ dplyr::last(Albuminuria_stages)
  )
}


#' Albuminuria Staging based on ACR
#'
#' Using KDIGO 2012 Clinical Practice Guideline for
#' the Evaluation and Management of Chronic Kidney Disease
#' Volume 3 | Issue 1 | January 2013
#'
#' \itemize{
#'   \item{A1: Normal to mildly increased}
#'   \item{A2: Moderately increased}
#'   \item{A3: Severely increased}
#' }
#'
#' See <https://kdigo.org/guidelines/ckd-evaluation-and-management/> for more details
#'
#' @param .data (data.frame) A data.frame, optional
#' @param ACR Albumin-to-creatinine ratio
#'   column name, or vector of units or numeric if `.data` is not provided
#' @param ... Further optional arguments
#'
#' @return Albuminuria category
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   ACR = units::set_units(c(-1, NA, 1, 10, 50), "mg/g")
#' )
#'
#' Albuminuria_staging_ACR(df, "ACR")
#'
#' df %>%
#'   dplyr::mutate(GFR_level = Albuminuria_staging_ACR(ACR))
Albuminuria_staging_ACR <- function(...) {
  UseMethod("Albuminuria_staging_ACR")
}

#' @rdname Albuminuria_staging_ACR
#' @export
Albuminuria_staging_ACR.data.frame <- function(.data, ACR, ...) {
  Albuminuria_staging_ACR(
    .data[[rlang::as_name(rlang::enquo(ACR))]]
  )
}

#' @rdname Albuminuria_staging_ACR
#' @export
Albuminuria_staging_ACR.units <- function(ACR, ...) {
  Albuminuria_staging_ACR(
    as_metric(ACR = ACR, value_only = TRUE)
  )
}

#' @rdname Albuminuria_staging_ACR
#' @export
Albuminuria_staging_ACR.numeric <- function(ACR, ...) {
  dplyr::case_when(
    ACR > 30 ~ Albuminuria_stages[3],
    ACR > 3 ~ Albuminuria_stages[2],
    ACR > 0 ~ Albuminuria_stages[1],
    TRUE ~ dplyr::last(Albuminuria_stages)
  )
}
