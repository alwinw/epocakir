test_that("as_metric() conversions are correct", {
  kdigo_factors <- tibble::tribble(
    ~parameter, ~factor, ~si_units,
    # General
    "Age", 12, "months",
    "height", 100, "cm",
    # 2012 AKI Guideline
    "SAmk", 1.708, "umol/l",
    "BUN", 0.357, "mmol/l",
    "SiCa", 0.25, "mmol/l",
    "SCr", 88.4, "umol/l",
    "CLcr", 0.01667, "ml/s",
    "CGen", 2.09, "umol/l",
    "Glc", 0.0555, "mmol/l",
    "Lac", 0.111, "mmol/l",
    "STob", 2.139, "umol/l",
    "Urea", 0.167, "mmol/l",
    # 2012 CKD Guideline
    "SAlb", 10, "g/l",
    "Hb", 10, "g/l",
    # "SPhos", 0.323, "mmol/l",
    # "SPTH", 0.106, "pmol/l",
    "UA", 59.485, "umol/l",
    # "VitD", 2.496, "nmol/l"
    "GFR", 1, "mL/min",
    "eGFR", 1, "mL/min/1.73m2",
    "SCysC", 1, "mg/l",
    "AER", 1, "mg/day",
    "ACR", 1, "mg/g",
    "UO", 1, "ml/kg"
  ) %>%
    dplyr::left_join(conversion_factors, ., by = "parameter") %>%
    dplyr::filter(!is.na(factor))

  expect_identical(nrow(conversion_factors), nrow(kdigo_factors))

  for (i in seq_len(nrow(kdigo_factors))) {
    expect_lte(
      abs(
        as_metric(
          param = kdigo_factors[[i, "parameter"]],
          meas = units::set_units(
            kdigo_factors[[i, "factor"]], kdigo_factors[[i, "si_units"]],
            mode = "standard"
          )
        ) - units::set_units(1, kdigo_factors[[i, "metric_units"]], mode = "standard")
      ),
      expected = units::set_units(5e-3, kdigo_factors[[i, "metric_units"]], mode = "standard"),
      label = paste(kdigo_factors[[i, "description"]], "conversion of", kdigo_factors[[i, "factor"]]),
      expected.label = "allowable tolerance"
    )
  }
})

test_that("as_metric() on single value", {
  expect_lte(
    abs(
      as_metric(param = "scr", meas = units::set_units(88.4, "umol/l")) -
        units::set_units(1, "mg/dl")
    ),
    expected = units::set_units(5e-3, "mg/dl")
  )

  expect_lte(
    abs(
      as_metric("SCr", units::set_units(88.4, "umol/l")) -
        units::set_units(1, "mg/dl")
    ),
    expected = units::set_units(5e-3, "mg/dl")
  )

  expect_lte(
    abs(
      as_metric(SCr = units::set_units(88.4, "umol/l")) -
        units::set_units(1, "mg/dl")
    ),
    expected = units::set_units(5e-3, "mg/dl")
  )
})

test_that("as_metric() on vector", {
  values <- units::set_units(c(88.4, 88.4, 88.4), "umol/l")

  expect_lte(
    abs(sum(
      as_metric(SCr = values) -
        units::set_units(1, "mg/dl")
    )),
    expected = units::set_units(5e-3, "mg/dl")
  )

  expect_lte(
    abs(sum(
      data.frame(meas = values) %>%
        dplyr::mutate(meas = as_metric(SCr = meas)) %>%
        dplyr::pull(meas) -
        units::set_units(1, "mg/dl")
    )),
    expected = units::set_units(5e-3, "mg/dl")
  )
})

test_that("as_metric() on NULL", {
  expect_null(as_metric(NULL))
  expect_null(as_metric(1))
})

test_that("as_metric() error on unknown measurement", {
  expect_error(as_metric(unknown = 1))
  expect_error(as_metric(param = "unknown", meas = 1))
})

test_that("dob2age() between two dates is valid", {
  expect_identical(
    dob2age(
      dob = lubridate::as_date("1990-01-01"),
      age_on = lubridate::as_date("2002-01-01")
    ),
    lubridate::duration(years = 12)
  )
})

test_that("dob2age() for a vector is valid", {
  expect_identical(
    dob2age(
      dob = c(
        lubridate::as_date("1990-01-01"),
        lubridate::as_date("1994-01-01"),
        lubridate::as_date("1998-01-01")
      ),
      age_on = lubridate::as_date("2002-01-01")
    ),
    c(
      lubridate::duration(years = 12),
      lubridate::duration(years = 8),
      lubridate::duration(years = 4)
    )
  )
})

test_that("dob2age() with `floor` is valid", {
  expect_identical(
    dob2age(
      dob = c(
        lubridate::as_date("1990-01-01"),
        lubridate::as_date("1994-01-01"),
        lubridate::as_date("1998-01-01")
      ),
      age_on = lubridate::as_date("2002-12-31"),
      fun = floor
    ),
    c(
      lubridate::duration(years = 12),
      lubridate::duration(years = 8),
      lubridate::duration(years = 4)
    )
  )
})

test_that("dob2age() with `ceiling` is valid", {
  expect_identical(
    dob2age(
      dob = c(
        lubridate::as_date("1990-01-01"),
        lubridate::as_date("1994-01-01"),
        lubridate::as_date("1998-01-01")
      ),
      age_on = lubridate::as_date("2001-12-31"),
      fun = ceiling
    ),
    c(
      lubridate::duration(years = 12),
      lubridate::duration(years = 8),
      lubridate::duration(years = 4)
    )
  )
})

test_that("binary2factor() with multiple columns", {
  df <- data.frame(
    a = c(1, 0, NA, 1, 0),
    b = c("y", "n", NA, "Y", "n"),
    c = c("yes", "no", NA, "Yes", "No"),
    d = c(TRUE, FALSE, NA, TRUE, FALSE),
    e = c(1, 2, 3, 4, 5)
  )
  ep <- data.frame(
    a = factor(c(1, 0, NA, 1, 0), levels = c(0, 1), labels = c("Not_a", "a"), ordered = TRUE),
    b = factor(c(1, 0, NA, 1, 0), levels = c(0, 1), labels = c("Not_b", "b"), ordered = TRUE),
    c = factor(c(1, 0, NA, 1, 0), levels = c(0, 1), labels = c("Not_c", "c"), ordered = TRUE),
    d = factor(c(1, 0, NA, 1, 0), levels = c(0, 1), labels = c("Not_d", "d"), ordered = TRUE),
    e = c(1, 2, 3, 4, 5)
  )
  expect_identical(binary2factor(df, a, b:d), ep)
  expect_identical(df %>% binary2factor(-e), ep)
})

test_that("combine_date_time_cols() for multiple columns", {
  df1 <- data.frame(
    date_a = as.Date(c("2020-01-01", "2020-01-02")),
    date_b = as.POSIXct(c("2020-02-01", "2020-02-02")),
    time_a = as.POSIXct(c("1900-01-01 01:01:01", "1900-01-01 02:02:02")),
    time_b = as.POSIXct(c("1900-01-01 01:01:01", "1900-01-01 02:02:02"))
  )
  df2 <- data.frame(
    a = c(1, 2), date_a = df1$date_a, time_a = df1$time_a,
    b = c(3, 4), date_b = df1$date_b, time_b = df1$time_b
  )
  o1 <- tibble::tibble(
    DateTime_a = as.POSIXct(c("2020-01-01 01:01:01", "2020-01-02 02:02:02"), tz = "UTC"),
    DateTime_b = as.POSIXct(c("2020-02-01 01:01:01", "2020-02-02 02:02:02"), tz = "UTC")
  )
  o2 <- tibble::tibble(
    a = c(1, 2),
    DateTime_a = as.POSIXct(c("2020-01-01 01:01:01", "2020-01-02 02:02:02")),
    b = c(3, 4),
    DateTime_b = as.POSIXct(c("2020-02-01 01:01:01", "2020-02-02 02:02:02"))
  )

  expect_identical(combine_date_time_cols(df1, tz = "UTC"), o1)
  expect_identical(combine_date_time_cols(df2), o2)
})


changes_raw_df <- function(env = parent.frame()) {
  tibble::tibble(
    pt_id_ = c(rep("pt1", 3 + 3), rep("pt2", 3)),
    dttm_ = c(
      seq(
        lubridate::as_datetime("2020-10-18 09:00:00", tz = "Australia/Melbourne"),
        lubridate::as_datetime("2020-10-20 09:00:00", tz = "Australia/Melbourne"),
        length.out = 3
      ),
      seq(
        lubridate::as_datetime("2020-10-23 09:00:00", tz = "Australia/Melbourne"),
        lubridate::as_datetime("2020-10-25 21:00:00", tz = "Australia/Melbourne"),
        length.out = 3
      ),
      seq(
        lubridate::as_datetime("2020-10-18 10:00:00", tz = "Australia/Melbourne"),
        lubridate::as_datetime("2020-10-19 10:00:00", tz = "Australia/Melbourne"),
        length.out = 3
      )
    ),
    SCr_ = c(
      units::set_units(seq(2.0, 3.0, by = 0.5), "mg/dl"),
      units::set_units(seq(3.5, 4.0, by = 0.25), "mg/dl"),
      units::set_units(seq(3.3, 3.5, by = 0.10), "mg/dl")
    ),
    bCr_ = c(
      rep(units::set_units(1.8, "mg/dl"), 3 + 3),
      rep(units::set_units(3.0, "mg/dl"), 3)
    )
  )
}

changes_rand_df <- function(env = parent.frame()) {
  changes_raw_df()[c(4, 6, 3, 8, 1, 2, 7, 9, 5), ]
}

changes_exp_df <- function(env = parent.frame()) {
  cbind(
    changes_raw_df()[
      c(2, 3, 3, 5, 6, 8, 9, 9),
      c("pt_id_", "dttm_", "SCr_")
    ],
    tibble::tibble(
      D.SCr_ = units::set_units(c(0.5, 0.5, 1.0, 0.25, 0.25, 0.1, 0.1, 0.2), "mg/dl"),
      D.dttm_ = lubridate::make_difftime(hours = c(24, 24, 48, 30, 30, 12, 12, 24))
    )
  ) %>%
    tibble::remove_rownames() %>%
    tibble::tibble()
}

test_that("combn_changes for data.frame", {
  df <- combn_changes(changes_rand_df(), "dttm_", "SCr_", "pt_id_")
  expect_equal(df, changes_exp_df())

  df <- combn_changes(changes_rand_df(), dttm_, SCr_, pt_id_)
  expect_equal(df, changes_exp_df())

  df <- changes_rand_df() %>%
    combn_changes(dttm_, SCr_, pt_id_)
  expect_equal(df, changes_exp_df())
})

test_that("combn_changes for POSIXct", {
  df <- combn_changes(
    changes_rand_df()$dttm_,
    changes_rand_df()$SCr_,
    changes_rand_df()$pt_id_
  )
  colnames(df) <- c("pt_id_", "dttm_", "SCr_", "D.SCr_", "D.dttm_")
  expect_equal(df, changes_exp_df())
})

test_that("combn_changes with n < m", {
  df <- changes_raw_df()[1:7, ]
  ep <- changes_exp_df()[1:5, ]
  ep$D.dttm_ <- as.difftime(as.numeric(ep$D.dttm_, units = "days"), units = "days")
  expect_equal(combn_changes(df, "dttm_", "SCr_", "pt_id_"), ep)
})
