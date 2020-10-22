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

Atest_that(".dob2age() between two dates is valid", {
  expect_equal(
    .dob2age(lubridate::as_date("1990-01-01"))
  )
})
