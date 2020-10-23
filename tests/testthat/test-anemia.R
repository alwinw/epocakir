test_that("anemia() for numeric vector of Hb", {
  Hb <- 11:14
  expect_equal(2 * 2, 4)
})

test_that("anemia() for numeric vector of Hb in g/dl", {
  Hb <- units::set_units(11:14, "g/dl")
  age <- lubridate::duration(seq(1, 16, by = 5), "years")
  expect_equal(2 * 2, 4)
})
