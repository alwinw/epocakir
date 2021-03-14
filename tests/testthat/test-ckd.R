eGFR_adult_df <- function(env = parent.frame()) {
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

eGFR_child_df <- function(env = parent.frame()) {
  tibble::tibble(
    SCr = units::set_units(0.5, "mg/dl"),
    height = units::set_units(1.2, "m"),
    BUN = units::set_units(0.8, "mg/dl"),
    SCysC = units::set_units(0.4, "mg/l")
  )
}

eGFR_tol <- function(env = parent.frame()) {
  units::set_units(0.05, "mL/min/1.73m2")
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

  df_str <- eGFR_adult_SCr(eGFR_adult_df(), "SCr", "Age", "male", "black")
  df_sym <- eGFR_adult_SCr(eGFR_adult_df(), SCr, Age, male, black)
  df_mut <- eGFR_adult_df() %>%
    dplyr::mutate(eGFR = eGFR_adult_SCr(SCr, Age, male, black)) %>%
    dplyr::pull(eGFR)
  df_uvec <- eGFR_adult_SCr(
    eGFR_adult_df()$SCr,
    eGFR_adult_df()$Age,
    eGFR_adult_df()$male,
    eGFR_adult_df()$black
  )
  df_nvec <- eGFR_adult_SCr(
    as.numeric(eGFR_adult_df()$SCr),
    as.numeric(eGFR_adult_df()$Age),
    as.numeric(eGFR_adult_df()$male),
    as.numeric(eGFR_adult_df()$black)
  )

  lapply(abs(df_str - ep), expect_lte, eGFR_tol())
  lapply(abs(df_sym - ep), expect_lte, eGFR_tol())
  lapply(abs(df_mut - ep), expect_lte, eGFR_tol())
  lapply(abs(df_uvec - ep), expect_lte, eGFR_tol())
  lapply(abs(df_nvec - as.numeric(ep)), expect_lte, as.numeric(eGFR_tol()))
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

  df_str <- eGFR_adult_SCysC(eGFR_adult_df(), "SCysC", "Age", "male")
  df_sym <- eGFR_adult_SCysC(eGFR_adult_df(), SCysC, Age, male)
  df_mut <- eGFR_adult_df() %>%
    dplyr::mutate(eGFR = eGFR_adult_SCysC(SCysC, Age, male)) %>%
    dplyr::pull(eGFR)
  df_uvec <- eGFR_adult_SCysC(
    eGFR_adult_df()$SCysC,
    eGFR_adult_df()$Age,
    eGFR_adult_df()$male
  )
  df_nvec <- eGFR_adult_SCysC(
    as.numeric(eGFR_adult_df()$SCysC),
    as.numeric(eGFR_adult_df()$Age),
    as.numeric(eGFR_adult_df()$male)
  )

  lapply(abs(df_str - ep), expect_lte, eGFR_tol())
  lapply(abs(df_sym - ep), expect_lte, eGFR_tol())
  lapply(abs(df_mut - ep), expect_lte, eGFR_tol())
  lapply(abs(df_uvec - ep), expect_lte, eGFR_tol())
  lapply(abs(df_nvec - as.numeric(ep)), expect_lte, as.numeric(eGFR_tol()))
})


test_that("eGFR_adult_SCr_SCysC()", {
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

  df_str <- eGFR_adult_SCr_SCysC(eGFR_adult_df(), "SCr", "SCysC", "Age", "male", "black")
  df_sym <- eGFR_adult_SCr_SCysC(eGFR_adult_df(), SCr, SCysC, Age, male, black)
  df_mut <- eGFR_adult_df() %>%
    dplyr::mutate(eGFR = eGFR_adult_SCr_SCysC(SCr, SCysC, Age, male, black)) %>%
    dplyr::pull(eGFR)
  df_uvec <- eGFR_adult_SCr_SCysC(
    eGFR_adult_df()$SCr,
    eGFR_adult_df()$SCysC,
    eGFR_adult_df()$Age,
    eGFR_adult_df()$male,
    eGFR_adult_df()$black
  )
  df_nvec <- eGFR_adult_SCr_SCysC(
    as.numeric(eGFR_adult_df()$SCr),
    as.numeric(eGFR_adult_df()$SCysC),
    as.numeric(eGFR_adult_df()$Age),
    as.numeric(eGFR_adult_df()$male),
    as.numeric(eGFR_adult_df()$black)
  )

  lapply(abs(df_str - ep), expect_lte, eGFR_tol())
  lapply(abs(df_sym - ep), expect_lte, eGFR_tol())
  lapply(abs(df_mut - ep), expect_lte, eGFR_tol())
  lapply(abs(df_uvec - ep), expect_lte, eGFR_tol())
  lapply(abs(df_nvec - as.numeric(ep)), expect_lte, as.numeric(eGFR_tol()))
})


test_that("eGFR_child_SCr()", {
  ep <- units::set_units(41.3 * (1.2 / 0.5), "mL/min/1.73m2")

  df_str <- eGFR_child_SCr(eGFR_child_df(), "SCr", "height")
  df_sym <- eGFR_child_SCr(eGFR_child_df(), SCr, height)
  df_mut <- eGFR_child_df() %>%
    dplyr::mutate(eGFR = eGFR_child_SCr(SCr, height)) %>%
    dplyr::pull(eGFR)
  df_uvec <- eGFR_child_SCr(
    eGFR_child_df()$SCr,
    eGFR_child_df()$height
  )
  df_nvec <- eGFR_child_SCr(
    as.numeric(eGFR_child_df()$SCr),
    as.numeric(eGFR_child_df()$height)
  )

  lapply(abs(df_str - ep), expect_lte, eGFR_tol())
  lapply(abs(df_sym - ep), expect_lte, eGFR_tol())
  lapply(abs(df_mut - ep), expect_lte, eGFR_tol())
  lapply(abs(df_uvec - ep), expect_lte, eGFR_tol())
  lapply(abs(df_nvec - as.numeric(ep)), expect_lte, as.numeric(eGFR_tol()))
})


test_that("eGFR_child_SCr_BUN()", {
  ep <- units::set_units(40.7 * (1.2 / 0.5)^0.64 * (30 / 0.8)^0.202, "mL/min/1.73m2")

  df_str <- eGFR_child_SCr_BUN(eGFR_child_df(), "SCr", "height", "BUN")
  df_sym <- eGFR_child_SCr_BUN(eGFR_child_df(), SCr, height, BUN)
  df_mut <- eGFR_child_df() %>%
    dplyr::mutate(eGFR = eGFR_child_SCr_BUN(SCr, height, BUN)) %>%
    dplyr::pull(eGFR)
  df_uvec <- eGFR_child_SCr_BUN(
    eGFR_child_df()$SCr,
    eGFR_child_df()$height,
    eGFR_child_df()$BUN
  )
  df_nvec <- eGFR_child_SCr_BUN(
    as.numeric(eGFR_child_df()$SCr),
    as.numeric(eGFR_child_df()$height),
    as.numeric(eGFR_child_df()$BUN)
  )

  lapply(abs(df_str - ep), expect_lte, eGFR_tol())
  lapply(abs(df_sym - ep), expect_lte, eGFR_tol())
  lapply(abs(df_mut - ep), expect_lte, eGFR_tol())
  lapply(abs(df_uvec - ep), expect_lte, eGFR_tol())
  lapply(abs(df_nvec - as.numeric(ep)), expect_lte, as.numeric(eGFR_tol()))
})


test_that("eGFR_child_SCysC()", {
  ep <- units::set_units(70.69 * (0.4)^-0.931, "mL/min/1.73m2")

  df_str <- eGFR_child_SCysC(eGFR_child_df(), "SCysC")
  df_sym <- eGFR_child_SCysC(eGFR_child_df(), SCysC)
  df_mut <- eGFR_child_df() %>%
    dplyr::mutate(eGFR = eGFR_child_SCysC(SCysC)) %>%
    dplyr::pull(eGFR)
  df_uvec <- eGFR_child_SCysC(
    eGFR_child_df()$SCysC
  )
  df_nvec <- eGFR_child_SCysC(
    as.numeric(eGFR_child_df()$SCysC)
  )

  lapply(abs(df_str - ep), expect_lte, eGFR_tol())
  lapply(abs(df_sym - ep), expect_lte, eGFR_tol())
  lapply(abs(df_mut - ep), expect_lte, eGFR_tol())
  lapply(abs(df_uvec - ep), expect_lte, eGFR_tol())
  lapply(abs(df_nvec - as.numeric(ep)), expect_lte, as.numeric(eGFR_tol()))
})
