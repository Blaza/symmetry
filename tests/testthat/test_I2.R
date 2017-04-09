test_that("I2 works for (1, 2)", {
  expect_equal(I2(1:2), 11/16)
})

test_that("I2 works for rnorm(50)", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(I2(X), 0.0245208, tolerance = 1e-8)
})

