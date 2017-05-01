test_that("I2HU works for (1, 2)", {
  expect_equal(I2HU(1:2), 3/4)
})

test_that("I2HU works for rnorm(50)", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(I2HU(X), 0.007902041, tolerance = 1e-8)
})

