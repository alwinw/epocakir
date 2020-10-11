test_that("aki() idenfies AKI stages", {
  a <- 1
  b <- 2
  expect_equal(aki(a, 0, b), 3)
})
