test_that("I2U works for (1, 2, 3, 4)", {
  expect_equal(I2U(1:4), 0)
})

test_that("I2U works for (1, 2, 3, 4, 5)", {
  expect_equal(I2U(1:5), 0)
})

test_that("I2U works for (-2, -1, 0, 1, 2)", {
  expect_equal(I2U(-2:2), 2/5)
})

test_that("I2U works for rnorm(50)", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(I2U(X), 0.005749023, tolerance = 1e-8)
})

