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

test_that("aki() for numeric vector of SCr with no baseline", {
  SCr <- seq(60, 200, by = 20)
  aki_stages <- forcats::fct_c(
    .aki_stages[length(.aki_stages)],
    .aki_stages[length(.aki_stages)],
    .aki_stages[1],
    .aki_stages[2],
    .aki_stages[2],
    .aki_stages[2],
    .aki_stages[3],
    .aki_stages[3]
  )
  expect_equal(aki(SCr), aki_stages)
})

test_that("aki() for numeric vector of SCr with baseline", {
  SCr <- seq(60, 200, by = 20)
  aki_stages <- forcats::fct_c(
    .aki_stages[length(.aki_stages)],
    .aki_stages[1],
    .aki_stages[2],
    .aki_stages[2],
    .aki_stages[2],
    .aki_stages[3],
    .aki_stages[3],
    .aki_stages[3]
  )
  expect_equal(aki(SCr, bCr = 50), aki_stages)
})

test_that("aki() for vector of SCr in mg/dl with no baseline", {
  SCr <- units::set_units(seq(1.5, 4.5, by = 0.5), "mg/dl")
  aki_stages <- forcats::fct_c(
    .aki_stages[length(.aki_stages)],
    .aki_stages[length(.aki_stages)],
    .aki_stages[1],
    .aki_stages[2],
    .aki_stages[2],
    .aki_stages[3],
    .aki_stages[3]
  )
  expect_equal(aki(SCr), aki_stages)
})

test_that("aki() for vector of SCr in mg/dl with baseline", {
  SCr <- units::set_units(seq(2.0, 4.5, by = 0.5), "mg/dl")
  bCr <- units::set_units(1.5, "mg/dl")
  aki_stages <- forcats::fct_c(
    .aki_stages[length(.aki_stages)],
    .aki_stages[1],
    .aki_stages[2],
    .aki_stages[2],
    .aki_stages[3],
    .aki_stages[3]
  )
  expect_equal(aki(SCr, bCr), aki_stages)
})

pt_id <- "pt_id_"
dttm <- "dttm_"
SCr <- "SCr_"
bCr <- "bCr_"
aki <- "aki"
data_ <- data.frame(
  pt_id_ = c(rep("pt1", 11 + 7), rep("pt2", 13)),
  dttm_ = c(
    seq(
      lubridate::as_datetime("2020-10-18 09:00:00", tz = "Australia/Melbourne"),
      lubridate::as_datetime("2020-10-20 09:00:00", tz = "Australia/Melbourne"),
      length.out = 11
    ),
    seq(
      lubridate::as_datetime("2020-10-23 09:00:00", tz = "Australia/Melbourne"),
      lubridate::as_datetime("2020-10-25 09:00:00", tz = "Australia/Melbourne"),
      length.out = 7
    ),
    seq(
      lubridate::as_datetime("2020-10-18 10:00:00", tz = "Australia/Melbourne"),
      lubridate::as_datetime("2020-10-29 10:00:00", tz = "Australia/Melbourne"),
      length.out = 13
    )
  ),
  SCr_ = c(
    units::set_units(seq(2.0, 4.5, by = 0.25), "mg/dl"),
    units::set_units(seq(2.5, 4.0, by = 0.25), "mg/dl"),
    units::set_units(seq(3.0, 4.2, by = 0.10), "mg/dl")
  ),
  bCr_ = c(
    rep(units::set_units(1.8, "mg/dl"), 11 + 7),
    rep(units::set_units(3.0, "mg/dl"), 13)
  )
)

data <- data_[sample(nrow(data_)), ]
