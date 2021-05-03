# consider using Table 7 as the test cases

aki_SCr_test_df <- function(env = parent.frame()) {
  tibble::tibble(
    SCr_measured = units::set_units(seq(2.0, 4.5, by = 0.5), "mg/dl"),
    bCr_measured = units::set_units(1.5, "mg/dl")
  )
}

aki_SCr_exp_df <- function(env = parent.frame()) {
  vctrs::vec_c(
    NA,
    aki_stages[1],
    aki_stages[2],
    aki_stages[2],
    aki_stages[3],
    aki_stages[3]
  )
}

test_that("aki_bCr() for data.frame", {
  expect_identical(aki_bCr(aki_SCr_test_df(), "SCr_measured", "bCr_measured"), aki_SCr_exp_df())
  expect_identical(aki_bCr(aki_SCr_test_df(), SCr_measured, bCr_measured), aki_SCr_exp_df())
})

test_that("aki_bCr() for units vector", {
  SCr_measured <- aki_SCr_test_df()$SCr_measured
  bCr_measured <- aki_SCr_test_df()$bCr_measured
  expect_identical(aki_bCr(SCr_measured, bCr_measured), aki_SCr_exp_df())
})

test_that("aki_bCr() for dplyr::mutate on units", {
  df <- aki_SCr_test_df() %>%
    dplyr::mutate(aki = aki_bCr(SCr_measured, bCr_measured))
  expect_identical(df$aki, aki_SCr_exp_df())
})

test_that("aki_bCr() for dplyr::mutate on numeric", {
  df <- aki_SCr_test_df() %>%
    dplyr::mutate(dplyr::across(everything(), as.numeric)) %>%
    dplyr::mutate(aki = aki_bCr(SCr_measured, bCr_measured))
  expect_identical(df$aki, aki_SCr_exp_df())
})


test_that("aki() for dataframe of SCr and bCr only", {
  pt_id <- "pt_id_"
  dttm <- "dttm_"
  SCr <- "SCr_"
  bCr <- "bCr_"
  aki <- "aki"
  data_ <- data.frame(
    pt_id_ = c(rep("pt1", 11 + 7), rep("pt2", 13)),
    dttm_ = c(
      seq(
        lubridate::as_datetime("2020-10-18 09:00:00", tz = "Australia/Melbourne"),
        lubridate::as_datetime("2020-10-20 09:00:00", tz = "Australia/Melbourne"),
        length.out = 11
      ),
      seq(
        lubridate::as_datetime("2020-10-23 09:00:00", tz = "Australia/Melbourne"),
        lubridate::as_datetime("2020-10-25 09:00:00", tz = "Australia/Melbourne"),
        length.out = 7
      ),
      seq(
        lubridate::as_datetime("2020-10-18 10:00:00", tz = "Australia/Melbourne"),
        lubridate::as_datetime("2020-10-29 10:00:00", tz = "Australia/Melbourne"),
        length.out = 13
      )
    ),
    SCr_ = c(
      units::set_units(seq(2.0, 4.5, by = 0.25), "mg/dl"),
      units::set_units(seq(2.5, 4.0, by = 0.25), "mg/dl"),
      units::set_units(seq(3.0, 4.2, by = 0.10), "mg/dl")
    ),
    bCr_ = c(
      rep(units::set_units(1.8, "mg/dl"), 11 + 7),
      rep(units::set_units(3.0, "mg/dl"), 13)
    )
  )

  data <- data_[sample(nrow(data_)), ]
})
