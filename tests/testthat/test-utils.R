test_that(".sCr2metric() conversion 88.4 umol/l to 1mg/dl is correct", {
  expect_lte(
    abs(.sCr2metric(units::set_units(88.4, "umol/l")) - units::set_units(1, "mg/dl")),
    units::set_units(1e-4, "mg/dl")
  )
})

test_that(".sCr2metric() conversion 353.6 umol/l to 4mg/dl is correct", {
  expect_lte(
    abs(.sCr2metric(units::set_units(353.6, "umol/l")) - units::set_units(4, "mg/dl")),
    units::set_units(1e-4, "mg/dl")
  )
})

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
