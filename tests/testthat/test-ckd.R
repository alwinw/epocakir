test_that("eGFR.adult.SCr()", {
  df <- tibble::tribble(
    ~SCr, ~Age, ~male, ~black,
    0.5, 20, FALSE, FALSE,
    0.5, 30, FALSE, TRUE,
    1.5, 20, FALSE, FALSE,
    1.5, 30, FALSE, TRUE,
    0.5, 20, TRUE, FALSE,
    0.5, 30, TRUE, TRUE,
    1.5, 20, TRUE, FALSE,
    1.5, 30, TRUE, TRUE,
  ) %>%
    dplyr::mutate(
      SCr = units::set_units(SCr, "mg/dl"),
      Age = units::set_units(Age, "years"),
      eGFR = eGFR.adult.SCr(SCr, Age, male, black)
    ) %>%
    dplyr::pull(eGFR)

  ep <- units::set_units(c(
    144 * (0.5 / 0.7)^-0.329 * 0.993^20,
    144 * (0.5 / 0.7)^-0.329 * 0.993^30 * 1.159,
    144 * (1.5 / 0.7)^-1.209 * 0.993^20,
    144 * (1.5 / 0.7)^-1.209 * 0.993^30 * 1.159,
    141 * (0.5 / 0.9)^-0.411 * 0.993^20,
    141 * (0.5 / 0.9)^-0.411 * 0.993^30 * 1.159,
    141 * (1.5 / 0.9)^-1.209 * 0.993^20,
    141 * (1.5 / 0.9)^-1.209 * 0.993^30 * 1.159
  ), "mL/min/1.73m2")

  expect_lte(max(abs(df - ep)), units::set_units(0.5, "mL/min/1.73m2"))
})


test_that("eGFR.adult.SCysC()", {
  df <- tibble::tribble(
    ~SCysC, ~Age, ~male,
    0.4, 20, FALSE,
    0.8, 30, FALSE,
    0.4, 20, TRUE,
    0.8, 30, TRUE
  ) %>%
    dplyr::mutate(
      SCysC = units::set_units(SCysC, "mg/l"),
      Age = units::set_units(Age, "years"),
      eGFR = eGFR.adult.SCysC(SCysC, Age, male)
    ) %>%
    dplyr::pull(eGFR)

  ep <- units::set_units(c(
    133 * (0.4 / 0.8)^-0.499 * 0.996^20 * 0.932,
    133 * (0.8 / 0.8)^-1.328 * 0.996^30 * 0.932,
    133 * (0.4 / 0.8)^-0.499 * 0.996^20,
    133 * (0.8 / 0.8)^-1.328 * 0.996^30
  ), "mL/min/1.73m2")
})

test_that("eGFR.adult.SCr_SCysC()", {
  # TODO this should be a common framework? e.g. adult <- tribble()
  df <- tibble::tribble(
    ~SCr, ~SCysC, ~Age, ~male, ~black
    0.5, 0.4, 20, FALSE, FALSE,
    0.5, 0.4, 20, FALSE, TRUE,
    0.5, 0.8, 20, FALSE, FALSE,
    0.5, 0.8, 20, FALSE, TRUE,
    1.5, 0.4, 20, FALSE, FALSE,
    1.5, 0.4, 20, FALSE, TRUE,
    1.5, 0.8, 20, FALSE, FALSE,
    1.5, 0.8, 20, FALSE, TRUE,
    0.5, 0.4, 20, TRUE, FALSE,
    0.5, 0.4, 20, TRUE, TRUE,
    0.5, 0.8, 20, TRUE, FALSE,
    0.5, 0.8, 20, TRUE, TRUE,
    1.5, 0.4, 20, TRUE, FALSE,
    1.5, 0.4, 20, TRUE, TRUE,
    1.5, 0.8, 20, TRUE, FALSE,
    1.5, 0.8, 20, TRUE, TRUE
  ) %>%
    dplyr::mutate(
      SCr = units::set_units(SCr, "mg/dl"),
      SCysC = units::set_units(SCysC, "mg/l"),
      Age = units::set_units(Age, "years"),
      eGFR = eGFR.adult.SCr_SCysC(SCr, SCysC, Age, male, black)
    ) %>%
    dplyr::pull(eGFR)

  ep <- units::set_units(c(
    133 * (0.4 / 0.8)^-0.499 * 0.996^20 * 0.932,
    133 * (0.8 / 0.8)^-1.328 * 0.996^30 * 0.932,
    133 * (0.4 / 0.8)^-0.499 * 0.996^20,
    133 * (0.8 / 0.8)^-1.328 * 0.996^30
  ), "mL/min/1.73m2")
})
