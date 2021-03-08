test_that("eGFR.adult.SCr()", {
  df <- tibble::tribble(
    ~SCr, ~Age, ~male, ~black,
    0.5, 30, FALSE, FALSE,
    0.5, 40, FALSE, TRUE,
    1.5, 30, FALSE, FALSE,
    1.5, 40, FALSE, TRUE
  ) %>%
    dplyr::mutate(
      SCr = units::set_units(SCr, "mg/dl"),
      Age = units::set_units(Age, "years"),
      eGFR = eGFR.adult.SCr(SCr, Age, male, black)
    ) %>%
    dplyr::pull(eGFR)

  ep <- units::set_units(c(
    144 * (0.5 / 0.7)^-0.329 * 0.993^30,
    144 * (0.5 / 0.7)^-0.329 * 0.993^40 * 1.159,
    141 * (1.5/0.7)^-1.209 * 0.993^30,
    141 * (1.5/0.7)^-1.209 * 0.993^40 * 1.159
  ), "mL/min/1.73m2")

  expect_lte(max(abs(df - ep)), units::set_units(1, "mL/min/1.73m2"))
})
