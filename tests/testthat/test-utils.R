test_that("as_metric() conversions are correct", {
  kdigo_factors <- tibble::tribble(
    ~parameter, ~factor, ~si_units,
    "SAmk", 1.708, "umol/l",
    "BUN", 0.357, "mmol/l",
    "SiCa", 0.25, "mmol/l",
    "SCr", 88.4, "umol/l",
    "CLcr", 0.01667, "ml/s",
    "CGen", 2.09, "umol/l",
    "Glc", 0.0555, "mmol/l",
    "Lac", 0.111, "mmol/l",
    "STob", 2.139, "umol/l",
    "Urea", 0.167, "mmol/l"
  ) %>%
    dplyr::left_join(conversion_factors, ., by = "parameter")

  for (i in 1:nrow(kdigo_factors)) {
    expect_lte(
      abs(
        as_metric(
          param = kdigo_factors[[i, "parameter"]],
          meas = units::set_units(
            kdigo_factors[[i, "factor"]], kdigo_factors[[i, "si_units"]],
            mode = "standard"
          )
        ) -
          units::set_units(1, kdigo_factors[[i, "metric_units"]], mode = "standard")
      ),
      units::set_units(5e-3, kdigo_factors[[i, "metric_units"]], mode = "standard"),
      label = paste(kdigo_factors[[i, "description"]], "conversion of", kdigo_factors[[i, "factor"]]),
      expected.label = "allowable tolerance"
    )
  }
})

# More as_metric() tests

test_that("dob2age() between two dates is valid", {
  expect_equal(
    dob2age(
      dob = lubridate::as_date("1990-01-01"),
      age_on = lubridate::as_date("2002-01-01")
    ),
    lubridate::duration(years = 12)
  )
})

test_that("dob2age() for a vector is valid", {
  expect_equal(
    dob2age(
      dob = c(
        lubridate::as_date("1990-01-01"),
        lubridate::as_date("1994-01-01"),
        lubridate::as_date("1998-01-01")
      ),
      age_on = lubridate::as_date("2002-01-01")
    ),
    c(
      lubridate::duration(years = 12),
      lubridate::duration(years = 8),
      lubridate::duration(years = 4)
    )
  )
})

test_that("dob2age() with `floor` is valid", {
  expect_equal(
    dob2age(
      dob = c(
        lubridate::as_date("1990-01-01"),
        lubridate::as_date("1994-01-01"),
        lubridate::as_date("1998-01-01")
      ),
      age_on = lubridate::as_date("2002-12-31"),
      fun = floor
    ),
    c(
      lubridate::duration(years = 12),
      lubridate::duration(years = 8),
      lubridate::duration(years = 4)
    )
  )
})

test_that("dob2age() with `ceiling` is valid", {
  expect_equal(
    dob2age(
      dob = c(
        lubridate::as_date("1990-01-01"),
        lubridate::as_date("1994-01-01"),
        lubridate::as_date("1998-01-01")
      ),
      age_on = lubridate::as_date("2001-12-31"),
      fun = ceiling
    ),
    c(
      lubridate::duration(years = 12),
      lubridate::duration(years = 8),
      lubridate::duration(years = 4)
    )
  )
})
