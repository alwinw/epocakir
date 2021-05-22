# consider using Table 7 as the test cases

aki_bCr_test_df <- function(env = parent.frame()) {
  tibble::tibble(
    SCr_measured = units::set_units(seq(2.0, 4.5, by = 0.5), "mg/dl"),
    bCr_measured = units::set_units(1.5, "mg/dl")
  )
}

aki_bCr_exp_df <- function(env = parent.frame()) {
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
  expect_identical(aki_bCr(aki_bCr_test_df(), "SCr_measured", "bCr_measured"), aki_bCr_exp_df())
  expect_identical(aki_bCr(aki_bCr_test_df(), SCr_measured, bCr_measured), aki_bCr_exp_df())
})

test_that("aki_bCr() for units vector", {
  SCr_measured <- aki_bCr_test_df()$SCr_measured
  bCr_measured <- aki_bCr_test_df()$bCr_measured
  expect_identical(aki_bCr(SCr_measured, bCr_measured), aki_bCr_exp_df())
})

test_that("aki_bCr() for dplyr::mutate on units", {
  df <- aki_bCr_test_df() %>%
    dplyr::mutate(aki = aki_bCr(SCr_measured, bCr_measured))
  expect_identical(df$aki, aki_bCr_exp_df())
})

test_that("aki_bCr() for dplyr::mutate on numeric", {
  df <- aki_bCr_test_df() %>%
    dplyr::mutate(dplyr::across(everything(), as.numeric)) %>%
    dplyr::mutate(aki = aki_bCr(SCr_measured, bCr_measured))
  expect_identical(df$aki, aki_bCr_exp_df())
})


aki_SCr_test_raw_df <- function(env = parent.frame()) {
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

aki_SCr_test_rand_df <- function(env = parent.frame()) {
  aki_SCr_test_raw_df()[c(4, 6, 3, 8, 1, 2, 7, 9, 5), ]
}


test_that("aki_SCr() for data.frame", {
  aki_SCr(aki_SCr_test_rand_df(), "SCr_", "dttm_", "pt_id_")
})
