context("K1")

test_that("K1 works for (1, 2, 3) and k=1", {
  expect_equal(K1(1:3, 1), 2/3)
})

test_that("K1 works for (1, 2, 3, 4) and k=1", {
  expect_equal(K1(1:4, 1), 2/3)
})

test_that("K1 works for rnorm(50) and k=1", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(K1(X, 1), 0.1771429, tolerance = 1e-7)
})

test_that("K1 works for rnorm(50) and k=2", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(K1(X, 2), 0.1928571, tolerance = 1e-7)
})
