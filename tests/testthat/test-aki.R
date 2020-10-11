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
