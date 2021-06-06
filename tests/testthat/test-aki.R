# consider adding KDIGO Table 7 as the test cases

aki_bCr_test_df <- function(env = parent.frame()) {
  tibble::tibble(
    SCr_ = units::set_units(seq(2.0, 4.5, by = 0.5), "mg/dl"),
    bCr_ = units::set_units(1.5, "mg/dl"),
    aki_bCr = vctrs::vec_c(
      dplyr::last(aki_stages),
      aki_stages[1],
      aki_stages[2],
      aki_stages[2],
      aki_stages[3],
      aki_stages[3]
    )
  )
}

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
      units::set_units(seq(3.4, 3.9, by = 0.25), "mg/dl"),
      units::set_units(seq(3.3, 3.5, by = 0.10), "mg/dl")
    ),
    bCr_ = c(
      rep(units::set_units(1.8, "mg/dl"), 3 + 3),
      rep(units::set_units(3.0, "mg/dl"), 3)
    ),
    aki_SCr = vctrs::vec_c(
      dplyr::last(aki_stages),
      rep(aki_stages[1], 2),
      rep(dplyr::last(aki_stages), 6)
    )
  )
}

aki_SCr_test_rand_df <- function(env = parent.frame()) {
  aki_SCr_test_raw_df()[c(4, 6, 3, 8, 1, 2, 7, 9, 5), ]
}

aki_UO_test_raw_df <- function(env = parent.frame()) {
  tibble::tribble(
    ~pt_id_, ~dttm_, ~UO_, ~aki_UO,
    3, "2020-10-18 09:00:00", 8, 4,
    3, "2020-10-18 15:00:00", 5, 4,
    3, "2020-10-18 21:00:00", 2, 1,
    3, "2020-10-19 01:00:00", 1, 1,
    3, "2020-10-19 03:00:00", 1, 2,
    3, "2020-10-19 09:00:00", 1, 2,
    3, "2020-10-19 15:00:00", 1, 3,
    3, "2020-10-19 21:00:00", 1, 3,
    4, "2020-10-18 12:00:00", 2, 4,
    4, "2020-10-18 18:00:00", 0, 1,
    4, "2020-10-19 00:00:00", 0, 3,
    4, "2020-10-19 06:00:00", 0, 3,
  ) %>%
    dplyr::mutate(
      pt_id_ = paste0("pt", pt_id_),
      dttm_ = lubridate::as_datetime(dttm_, tz = "Australia/Melbourne"),
      UO_ = units::set_units(UO_, "ml/kg"),
      aki_UO = aki_stages[aki_UO]
    )
}

aki_UO_test_rand_df <- function(env = parent.frame()) {
  aki_UO_test_raw_df()[c(10, 9, 5, 8, 3, 11, 6, 2, 4, 12, 7, 1), ]
}

aki_test_df <- function(env = parent.frame()) {
  dplyr::bind_rows(aki_bCr_test_df(), aki_SCr_test_rand_df(), aki_UO_test_rand_df()) %>%
    tidyr::pivot_longer(
      dplyr::starts_with("aki_"),
      names_to = "aki_staging_type",
      values_to = "aki_"
    ) %>%
    dplyr::mutate(
      SCr_ = dplyr::if_else(grepl("bCr|SCr", aki_staging_type), SCr_, NA_real_),
      bCr_ = dplyr::if_else(grepl("bCr", aki_staging_type), bCr_, NA_real_),
      UO_ = dplyr::if_else(grepl("UO", aki_staging_type), UO_, NA_real_)
    ) %>%
    dplyr::filter(!is.na(aki_))
}


test_that("aki_staging() on full aki_test_df()", {
  ep <- aki_test_df()$aki_

  df_str <- aki_staging(aki_test_df(),
    SCr = "SCr_", bCr = "bCr_", UO = "UO_", dttm = "dttm_", pt_id = "pt_id_"
  )
  df_mut <- aki_test_df() %>%
    dplyr::mutate(aki = aki_staging(
      SCr = SCr_, bCr = bCr_, UO = UO_, dttm = dttm_, pt_id = pt_id_
    )) %>%
    dplyr::pull(aki)
  df_uvec <- aki_staging(
    aki_test_df()$SCr_,
    aki_test_df()$bCr_,
    aki_test_df()$UO_,
    aki_test_df()$dttm_,
    aki_test_df()$pt_id_
  )
  df_nvec <- aki_staging(
    as.numeric(aki_test_df()$SCr_),
    as.numeric(aki_test_df()$bCr_),
    as.numeric(aki_test_df()$UO_),
    aki_test_df()$dttm_,
    aki_test_df()$pt_id_
  )

  expect_identical(df_str, ep)
  expect_identical(df_mut, ep)
  expect_identical(df_uvec, ep)
  expect_identical(df_nvec, ep)
})

test_that("aki_staging() on individual data.frames", {
  expect_identical(
    aki_staging(aki_bCr_test_df(), SCr = "SCr_", bCr = "bCr_"),
    aki_bCr_test_df()$aki_bCr
  )
  expect_identical(
    aki_staging(aki_SCr_test_rand_df(), SCr = "SCr_", dttm = "dttm_", pt_id = "pt_id_"),
    aki_SCr_test_rand_df()$aki_SCr
  )
  expect_identical(
    aki_staging(aki_UO_test_rand_df(), UO = "UO_", dttm = "dttm_", pt_id = "pt_id_"),
    aki_UO_test_rand_df()$aki_UO
  )
})

test_that("aki_staging() warnings", {
  df_no_pt_id <- aki_SCr_test_raw_df() %>%
    dplyr::filter(pt_id_ == "pt1")
  expect_warning(
    aki_staging(df_no_pt_id, SCr = "SCr_", dttm = "dttm_"),
    ".*Assuming provided data is for a single patient"
  )
  expect_identical(
    suppressWarnings(aki_staging(df_no_pt_id, SCr = "SCr_", dttm = "dttm_")),
    df_no_pt_id$aki_SCr
  )
})


test_that("aki_bCr() for data.frame", {
  expect_identical(aki_bCr(aki_bCr_test_df(), "SCr_", "bCr_"), aki_bCr_test_df()$aki_bCr)
  expect_identical(aki_bCr(aki_bCr_test_df(), SCr_, bCr_), aki_bCr_test_df()$aki_bCr)
})

test_that("aki_bCr() for units vector", {
  SCr_ <- aki_bCr_test_df()$SCr_
  bCr_ <- aki_bCr_test_df()$bCr_
  expect_identical(aki_bCr(SCr_, bCr_), aki_bCr_test_df()$aki_bCr)
})

test_that("aki_bCr() for dplyr::mutate on units", {
  df <- aki_bCr_test_df() %>%
    dplyr::mutate(aki = aki_bCr(SCr_, bCr_))
  expect_identical(df$aki, aki_bCr_test_df()$aki_bCr)
})

test_that("aki_bCr() for dplyr::mutate on numeric", {
  df <- aki_bCr_test_df() %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.numeric)) %>%
    dplyr::mutate(aki = aki_bCr(SCr_, bCr_))
  expect_identical(df$aki, aki_bCr_test_df()$aki_bCr)
})


test_that("aki_SCr() for data.frame", {
  expect_identical(aki_SCr(aki_SCr_test_rand_df(), "SCr_", "dttm_", "pt_id_"), aki_SCr_test_rand_df()$aki_SCr)
  expect_identical(aki_SCr(aki_SCr_test_rand_df(), SCr_, dttm_, pt_id_), aki_SCr_test_rand_df()$aki_SCr)
})

test_that("aki_SCr() for units vector", {
  SCr_ <- aki_SCr_test_rand_df()$SCr_
  dttm_ <- aki_SCr_test_rand_df()$dttm_
  pt_id_ <- aki_SCr_test_rand_df()$pt_id_
  expect_identical(aki_SCr(SCr_, dttm_, pt_id_), aki_SCr_test_rand_df()$aki_SCr)
})

test_that("aki_SCr() for dplyr::mutate on units", {
  df <- aki_SCr_test_rand_df() %>%
    dplyr::mutate(aki = aki_SCr(SCr_, dttm_, pt_id_))
  expect_identical(df$aki, aki_SCr_test_rand_df()$aki_SCr)
})

test_that("aki_bCr() for dplyr::mutate on numeric", {
  df <- aki_SCr_test_rand_df() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), as.numeric)) %>%
    dplyr::mutate(aki = aki_SCr(SCr_, dttm_, pt_id_))
  expect_identical(df$aki, aki_SCr_test_rand_df()$aki_SCr)
})


test_that("aki_UO() for data.frame", {
  expect_identical(aki_UO(aki_UO_test_rand_df(), "UO_", "dttm_", "pt_id_"), aki_UO_test_rand_df()$aki_UO)
  expect_identical(aki_UO(aki_UO_test_rand_df(), UO_, dttm_, pt_id_), aki_UO_test_rand_df()$aki_UO)
})

test_that("aki_UO() for units vector", {
  UO_ <- aki_UO_test_rand_df()$UO_
  dttm_ <- aki_UO_test_rand_df()$dttm_
  pt_id_ <- aki_UO_test_rand_df()$pt_id_
  expect_identical(aki_UO(UO_, dttm_, pt_id_), aki_UO_test_rand_df()$aki_UO)
})

test_that("aki_UO() for dplyr::mutate on units", {
  df <- aki_UO_test_rand_df() %>%
    dplyr::mutate(aki = aki_UO(UO_, dttm_, pt_id_))
  expect_identical(df$aki, aki_UO_test_rand_df()$aki_UO)
})

test_that("aki_UO() for dplyr::mutate on numeric", {
  df <- aki_UO_test_rand_df() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), as.numeric)) %>%
    dplyr::mutate(aki = aki_UO(UO_, dttm_, pt_id_))
  expect_identical(df$aki, aki_UO_test_rand_df()$aki_UO)
})
