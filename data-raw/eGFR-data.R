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
      dplyr::starts_with("eGFR_"),
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


# Consider finding an alternative with real, open access data instead
eGFR_pt_data <- eGFR_df()

usethis::use_data(eGFR_pt_data, overwrite = TRUE)
