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
    ) %>%
    tibble::add_column(
      eGFR_adult_SCr = c(
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
      ),
      eGFR_adult_SCysC = c(
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
      ),
      eGFR_adult_SCr_SCysC = c(
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
      )
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("eGFR"), units::set_units, "mL/min/1.73m2"
    ))
}

eGFR_child_df <- function(env = parent.frame()) {
  tibble::tibble(
    SCr = units::set_units(0.5, "mg/dl"),
    height = units::set_units(1.2, "m"),
    BUN = units::set_units(0.8, "mg/dl"),
    SCysC = units::set_units(0.4, "mg/l")
  ) %>%
    tibble::add_column(
      eGFR_child_SCr = 41.3 * (1.2 / 0.5),
      eGFR_child_SCr_BUN = 40.7 * (1.2 / 0.5)^0.64 * (30 / 0.8)^0.202,
      eGFR_child_SCysC = 70.69 * (0.4)^-0.931
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("eGFR"), units::set_units, "mL/min/1.73m2"
    ))
}

eGFR_df <- function(env = parent.frame()) {
  dplyr::bind_rows(eGFR_adult_df(), eGFR_child_df()) %>%
    tidyr::pivot_longer(
      dplyr::starts_with("eGFR"),
      names_to = "eGFR_calc_type",
      values_to = "eGFR"
    ) %>%
    dplyr::mutate(
      pediatric = grepl("child", eGFR_calc_type),
      SCr = dplyr::if_else(grepl("SCr", eGFR_calc_type), SCr, NA_real_),
      SCysC = dplyr::if_else(grepl("SCysC", eGFR_calc_type), SCysC, NA_real_),
      BUN = dplyr::if_else(grepl("BUN", eGFR_calc_type), BUN, NA_real_),
      Age = dplyr::if_else(pediatric, units::set_units(10, "years"), Age)
    ) %>%
    dplyr::filter(!is.na(eGFR)) %>%
    dplyr::rename_with(~ paste0(.x, "_"))
}

eGFR_tol <- function(env = parent.frame()) {
  units::set_units(0.05, "mL/min/1.73m2")
}

test_that("eGFR() on full eGFR_df()", {
  ep <- eGFR_df()$eGFR_

  df_str <- eGFR(eGFR_df(),
    SCr = "SCr_", SCysC = "SCysC_",
    Age = "Age_", height = "height_", BUN = "BUN_",
    male = "male_", black = "black_", pediatric = "pediatric_"
  )
  # df_sym <- eGFR(eGFR_df(),
  #   SCr = SCr_, SCysC = SCysC_,
  #   Age = Age_, height = height_, BUN = BUN_,
  #   male = male_, black = black_, pediatric = pediatric_
  # )
  df_mut <- eGFR_df() %>%
    dplyr::mutate(eGFR = eGFR(
      SCr = SCr_, SCysC = SCysC_,
      Age = Age_, height = height_, BUN = BUN_,
      male = male_, black = black_, pediatric = pediatric_
    )) %>%
    dplyr::pull(eGFR)
  df_uvec <- eGFR(
    eGFR_df()$SCr_,
    eGFR_df()$SCysC_,
    eGFR_df()$Age_,
    eGFR_df()$height_,
    eGFR_df()$BUN_,
    eGFR_df()$male_,
    eGFR_df()$black_,
    eGFR_df()$pediatric_
  )
  df_nvec <- eGFR(
    as.numeric(eGFR_df()$SCr_),
    as.numeric(eGFR_df()$SCysC_),
    as.numeric(eGFR_df()$Age_),
    as.numeric(eGFR_df()$height_),
    as.numeric(eGFR_df()$BUN_),
    as.numeric(eGFR_df()$male_),
    as.numeric(eGFR_df()$black_),
    as.numeric(eGFR_df()$pediatric_)
  )

  lapply(abs(df_str - ep), expect_lte, eGFR_tol())
  # lapply(abs(df_sym - ep), expect_lte, eGFR_tol())
  lapply(abs(df_mut - ep), expect_lte, eGFR_tol())
  lapply(abs(df_uvec - ep), expect_lte, eGFR_tol())
  lapply(abs(df_nvec - as.numeric(ep)), expect_lte, as.numeric(eGFR_tol()))
})


test_that("eGFR_adult_SCr()", {
  ep <- eGFR_adult_df()$eGFR_adult_SCr

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
  ep <- eGFR_adult_df()$eGFR_adult_SCysC

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
  ep <- eGFR_adult_df()$eGFR_adult_SCr_SCysC

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
  ep <- eGFR_child_df()$eGFR_child_SCr

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
  ep <- eGFR_child_df()$eGFR_child_SCr_BUN

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
  ep <- eGFR_child_df()$eGFR_child_SCysC

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


test_that("GFR_staging()", {
  ep <- vctrs::vec_c(NA, NA, GFR_stages)

  df <- tibble::tibble(
    eGFR = units::set_units(c(-1, NA, 100, 70, 50, 35, 20, 10), "mL/min/1.73m2")
  )
  df_str <- GFR_staging(df, "eGFR")
  df_sym <- GFR_staging(df, eGFR)
  df_mut <- df %>%
    dplyr::mutate(GFR_level = GFR_staging(eGFR)) %>%
    dplyr::pull(GFR_level)
  df_uvec <- GFR_staging(df$eGFR)
  df_nvec <- GFR_staging(as.numeric(df$eGFR))

  expect_identical(df_str, ep)
  expect_identical(df_sym, ep)
  expect_identical(df_mut, ep)
  expect_identical(df_uvec, ep)
  expect_identical(df_nvec, ep)
})


test_that("Albuminuria_staging_AER()", {
  ep <- vctrs::vec_c(NA, NA, Albuminuria_stages)

  df <- tibble::tibble(
    AER = units::set_units(c(-1, NA, 15, 100, 500), "mg/day")
  )
  df_str <- Albuminuria_staging_AER(df, "AER")
  df_sym <- Albuminuria_staging_AER(df, AER)
  df_mut <- df %>%
    dplyr::mutate(GFR_level = Albuminuria_staging_AER(AER)) %>%
    dplyr::pull(GFR_level)
  df_uvec <- Albuminuria_staging_AER(df$AER)
  df_nvec <- Albuminuria_staging_AER(as.numeric(df$AER))

  expect_identical(df_str, ep)
  expect_identical(df_sym, ep)
  expect_identical(df_mut, ep)
  expect_identical(df_uvec, ep)
  expect_identical(df_nvec, ep)
})


test_that("Albuminuria_staging_ACR()", {
  ep <- vctrs::vec_c(NA, NA, Albuminuria_stages)

  df <- tibble::tibble(
    ACR = units::set_units(c(-1, NA, 1, 10, 50), "mg/g")
  )
  df_str <- Albuminuria_staging_ACR(df, "ACR")
  df_sym <- Albuminuria_staging_ACR(df, ACR)
  df_mut <- df %>%
    dplyr::mutate(GFR_level = Albuminuria_staging_ACR(ACR)) %>%
    dplyr::pull(GFR_level)
  df_uvec <- Albuminuria_staging_ACR(df$ACR)
  df_nvec <- Albuminuria_staging_ACR(as.numeric(df$ACR))

  expect_identical(df_str, ep)
  expect_identical(df_sym, ep)
  expect_identical(df_mut, ep)
  expect_identical(df_uvec, ep)
  expect_identical(df_nvec, ep)
})
