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

# Consider finding an alternative with real, open access data instead
aki_pt_data <- aki_test_df()

usethis::use_data(aki_pt_data, overwrite = TRUE)
