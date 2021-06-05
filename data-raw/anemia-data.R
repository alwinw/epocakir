anemia_df <- function(env = parent.frame()) {
  tibble::tribble(
    ~Hb, ~age, ~male,
    12.5, 20, TRUE,
    13.5, 20, TRUE,
    11.5, 20, FALSE,
    12.5, 20, FALSE,
    11.5, 13, TRUE,
    12.5, 13, TRUE,
    11.0, 7, TRUE,
    12.0, 7, TRUE,
    10.5, 3, TRUE,
    11.5, 3, TRUE,
  ) %>%
    dplyr::mutate(
      Hb = units::set_units(Hb, "g/dl"),
      age = units::set_units(age, "years")
    )
}

# Consider finding an alternative with real, open access data instead
anemia_pt_data <- anemia_df()

usethis::use_data(anemia_pt_data, overwrite = TRUE)
