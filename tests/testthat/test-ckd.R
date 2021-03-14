eGFR.adult.df <- function(env = parent.frame()) {
  tibble::tribble(
    ~SCr, ~SCysC, ~Age, ~male, ~black,
    0.5, 0.4, 20, FALSE, FALSE,
    0.5, 0.4, 30, FALSE, TRUE,
    0.5, 1.2, 20, FALSE, FALSE,
    0.5, 1.2, 30, FALSE, TRUE,
    1.5, 0.4, 20, FALSE, FALSE,
    1.5, 0.4, 30, FALSE, TRUE,
    1.5, 1.2, 20, FALSE, FALSE,
    1.5, 1.2, 30, FALSE, TRUE,
    0.5, 0.4, 20, TRUE, FALSE,
    0.5, 0.4, 30, TRUE, TRUE,
    0.5, 1.2, 20, TRUE, FALSE,
    0.5, 1.2, 30, TRUE, TRUE,
    1.5, 0.4, 20, TRUE, FALSE,
    1.5, 0.4, 30, TRUE, TRUE,
    1.5, 1.2, 20, TRUE, FALSE,
    1.5, 1.2, 30, TRUE, TRUE
  ) %>%
    dplyr::mutate(
      SCr = units::set_units(SCr, "mg/dl"),
      SCysC = units::set_units(SCysC, "mg/l"),
      Age = units::set_units(Age, "years")
    )
}


test_that("eGFR_adult_SCr()", {
  ep <- units::set_units(c(
    rep(c(
      143.5 * (0.5 / 0.7)^-0.329 * 0.993^20,
      143.5 * (0.5 / 0.7)^-0.329 * 0.993^30 * 1.159
    ), 2),
    rep(c(
      143.5 * (1.5 / 0.7)^-1.209 * 0.993^20,
      143.5 * (1.5 / 0.7)^-1.209 * 0.993^30 * 1.159
    ), 2),
    rep(c(
      141 * (0.5 / 0.9)^-0.411 * 0.993^20,
      141 * (0.5 / 0.9)^-0.411 * 0.993^30 * 1.159
    ), 2),
    rep(c(
      141 * (1.5 / 0.9)^-1.209 * 0.993^20,
      141 * (1.5 / 0.9)^-1.209 * 0.993^30 * 1.159
    ), 2)
  ), "mL/min/1.73m2")

  df_str <- eGFR_adult_SCr(eGFR.adult.df(), "SCr", "Age", "male", "black")
  df_sym <- eGFR_adult_SCr(eGFR.adult.df(), SCr, Age, male, black)
  df_mut <- eGFR.adult.df() %>%
    dplyr::mutate(eGFR = eGFR_adult_SCr(SCr, Age, male, black)) %>%
    dplyr::pull(eGFR)
  df_uvec <- eGFR_adult_SCr(
    eGFR.adult.df()$SCr,
    eGFR.adult.df()$Age,
    eGFR.adult.df()$male,
    eGFR.adult.df()$black
  )
  df_nvec <- eGFR_adult_SCr(
    as.numeric(eGFR.adult.df()$SCr),
    as.numeric(eGFR.adult.df()$Age),
    as.numeric(eGFR.adult.df()$male),
    as.numeric(eGFR.adult.df()$black)
  )

  lapply(abs(df_str - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_sym - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_mut - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_uvec - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_nvec - as.numeric(ep)), expect_lte, 0.2)
})


test_that("eGFR_adult_SCysC()", {
  ep <- units::set_units(c(
    rep(c(
      133 * (0.4 / 0.8)^-0.499 * 0.996^20 * 0.932,
      133 * (0.4 / 0.8)^-0.499 * 0.996^30 * 0.932,
      133 * (1.2 / 0.8)^-1.328 * 0.996^20 * 0.932,
      133 * (1.2 / 0.8)^-1.328 * 0.996^30 * 0.932
    ), 2),
    rep(c(
      133 * (0.4 / 0.8)^-0.499 * 0.996^20,
      133 * (0.4 / 0.8)^-0.499 * 0.996^30,
      133 * (1.2 / 0.8)^-1.328 * 0.996^20,
      133 * (1.2 / 0.8)^-1.328 * 0.996^30
    ), 2)
  ), "mL/min/1.73m2")

  df_str <- eGFR_adult_SCysC(eGFR.adult.df(), "SCysC", "Age", "male")
  df_sym <- eGFR_adult_SCysC(eGFR.adult.df(), SCysC, Age, male)
  df_mut <- eGFR.adult.df() %>%
    dplyr::mutate(eGFR = eGFR_adult_SCysC(SCysC, Age, male)) %>%
    dplyr::pull(eGFR)
  df_uvec <- eGFR_adult_SCysC(
    eGFR.adult.df()$SCysC,
    eGFR.adult.df()$Age,
    eGFR.adult.df()$male
  )
  df_nvec <- eGFR_adult_SCysC(
    as.numeric(eGFR.adult.df()$SCysC),
    as.numeric(eGFR.adult.df()$Age),
    as.numeric(eGFR.adult.df()$male)
  )

  lapply(abs(df_str - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_sym - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_mut - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_uvec - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_nvec - as.numeric(ep)), expect_lte, 0.2)
})


test_that("eGFR_adult_SCr_SCysC()", {
  df <- eGFR.adult.df() %>%
    dplyr::mutate(eGFR = eGFR_adult_SCr_SCysC(SCr, SCysC, Age, male, black)) %>%
    dplyr::pull(eGFR)

  ep <- units::set_units(c(
    130.8 * (0.5 / 0.7)^-0.248 * (0.4 / 0.8)^-0.375 * 0.995^20,
    130.8 * (0.5 / 0.7)^-0.248 * (0.4 / 0.8)^-0.375 * 0.995^30 * 1.08,
    130.8 * (0.5 / 0.7)^-0.248 * (1.2 / 0.8)^-0.711 * 0.995^20,
    130.8 * (0.5 / 0.7)^-0.248 * (1.2 / 0.8)^-0.711 * 0.995^30 * 1.08,
    130.8 * (1.5 / 0.7)^-0.601 * (0.4 / 0.8)^-0.375 * 0.995^20,
    130.8 * (1.5 / 0.7)^-0.601 * (0.4 / 0.8)^-0.375 * 0.995^30 * 1.08,
    130.8 * (1.5 / 0.7)^-0.601 * (1.2 / 0.8)^-0.711 * 0.995^20,
    130.8 * (1.5 / 0.7)^-0.601 * (1.2 / 0.8)^-0.711 * 0.995^30 * 1.08,
    135 * (0.5 / 0.9)^-0.207 * (0.4 / 0.8)^-0.375 * 0.995^20,
    135 * (0.5 / 0.9)^-0.207 * (0.4 / 0.8)^-0.375 * 0.995^30 * 1.08,
    135 * (0.5 / 0.9)^-0.207 * (1.2 / 0.8)^-0.711 * 0.995^20,
    135 * (0.5 / 0.9)^-0.207 * (1.2 / 0.8)^-0.711 * 0.995^30 * 1.08,
    135 * (1.5 / 0.9)^-0.601 * (0.4 / 0.8)^-0.375 * 0.995^20,
    135 * (1.5 / 0.9)^-0.601 * (0.4 / 0.8)^-0.375 * 0.995^30 * 1.08,
    135 * (1.5 / 0.9)^-0.601 * (1.2 / 0.8)^-0.711 * 0.995^20,
    135 * (1.5 / 0.9)^-0.601 * (1.2 / 0.8)^-0.711 * 0.995^30 * 1.08
  ), "mL/min/1.73m2")

  df_str <- eGFR_adult_SCr_SCysC(eGFR.adult.df(), "SCr", "SCysC", "Age", "male", "black")
  df_sym <- eGFR_adult_SCr_SCysC(eGFR.adult.df(), SCr, SCysC, Age, male, black)
  df_mut <- eGFR.adult.df() %>%
    dplyr::mutate(eGFR = eGFR_adult_SCr_SCysC(SCr, SCysC, Age, male, black)) %>%
    dplyr::pull(eGFR)
  df_uvec <- eGFR_adult_SCr_SCysC(
    eGFR.adult.df()$SCr,
    eGFR.adult.df()$SCysC,
    eGFR.adult.df()$Age,
    eGFR.adult.df()$male,
    eGFR.adult.df()$black
  )
  df_nvec <- eGFR_adult_SCr_SCysC(
    as.numeric(eGFR.adult.df()$SCr),
    as.numeric(eGFR.adult.df()$SCysC),
    as.numeric(eGFR.adult.df()$Age),
    as.numeric(eGFR.adult.df()$male),
    as.numeric(eGFR.adult.df()$black)
  )

  lapply(abs(df_str - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_sym - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_mut - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_uvec - ep), expect_lte, units::set_units(0.2, "mL/min/1.73m2"))
  lapply(abs(df_nvec - as.numeric(ep)), expect_lte, 0.2)
})


test_that("eGFR_child_SCr()", {
  df <- tibble::tibble(
    SCr = units::set_units(0.5, "mg/dl"),
    height = units::set_units(1.2, "m")
  ) %>%
    dplyr::mutate(eGFR = eGFR_child_SCr(SCr, height)) %>%
    dplyr::pull(eGFR)

  ep <- units::set_units(41.3 * (1.2 / 0.5), "mL/min/1.73m2")
  expect_lte(abs(df - ep), units::set_units(0.2, "mL/min/17.3m2"))
})


test_that("eGFR_child_SCr_BUN", {
  df <- tibble::tibble(
    SCr = units::set_units(0.5, "mg/dl"),
    height = units::set_units(1.2, "m"),
    BUN = units::set_units(0.8, "mg/dl")
  ) %>%
    dplyr::mutate(eGFR = eGFR_child_SCr_BUN(SCr, height, BUN)) %>%
    dplyr::pull(eGFR)

  ep <- units::set_units(40.7 * (1.2 / 0.5)^0.64 * (30 / 0.8)^0.202, "mL/min/1.73m2")
  expect_lte(abs(df - ep), units::set_units(0.2, "mL/min/17.3m2"))
})


test_that("eGFR_child_SCysC", {
  df <- tibble::tibble(
    SCysC = units::set_units(0.4, "mg/l"),
  ) %>%
    dplyr::mutate(eGFR = eGFR_child_SCysC(SCysC)) %>%
    dplyr::pull(eGFR)

  ep <- units::set_units(70.69 * (0.4)^-0.931, "mL/min/1.73m2")
  expect_lte(abs(df - ep), units::set_units(0.2, "mL/min/17.3m2"))
})
