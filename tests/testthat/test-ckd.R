test_that("eGFR.adult.SCr()", {
  df <- tibble::tibble(
    a1 = eGFR.adult.SCr(
      SCr = units::set_units(0.5, "mg/dl"),
      Age = units::set_units(30, "years"),
      male = FALSE,
      black = FALSE
    ),
    a2 = eGFR.adult.SCr(
      SCr = units::set_units(0.5, "mg/dl"),
      Age = units::set_units(30, "years"),
      male = FALSE,
      black = TRUE
    ),
  ) %>% dplyr::mutate(
    dplyr::across(dplyr::everything(), round, 0)
  )
  ep <- tibble::tibble(
    a1 = 144 * (0.5 / 0.7)^-0.329 * 0.993^30,
    a2 = 144 * (0.5 / 0.7)^-0.329 * 0.993^30 * 1.159,
  ) %>% dplyr::mutate(
    dplyr::across(dplyr::everything(), round, 0)
  )
  expect_equal(df, ep)
})
