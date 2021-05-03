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

anemia_ep <- function(env = parent.frame()) {
  rep(c(TRUE, FALSE), 5)
}

test_that("anemia() for data.frame", {
  expect_identical(
    anemia(anemia_df(), "Hb", "age", "male"),
    anemia_ep()
  )
  expect_identical(
    anemia(anemia_df(), Hb, age, male),
    anemia_ep()
  )
  expect_identical( # TODO add this to all cases
    anemia_df() %>% anemia(Hb, age, male),
    anemia_ep()
  )
})

test_that("anemia() for units vector", {
  expect_identical(
    anemia(
      anemia_df()$Hb,
      anemia_df()$age,
      anemia_df()$male
    ),
    anemia_ep()
  )
})

test_that("anemia() for dplyr::mutate on units", {
  expect_identical(
    anemia_df() %>%
      dplyr::mutate(anemia = anemia(Hb, age, male)) %>%
      dplyr::pull(anemia),
    anemia_ep()
  )
})

test_that("anemia() for dplyr::mutate on numeric", {
  expect_identical(
    anemia_df() %>%
      dplyr::mutate(dplyr::across(c(Hb, age), as.numeric)) %>%
      dplyr::mutate(anemia = anemia(Hb, age, male)) %>%
      dplyr::pull(anemia),
    anemia_ep()
  )
})
