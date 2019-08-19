context("MI")

test_that("MI works for (1, 2, 3) and k=1", {
  expect_equal(MI(1:3, k=1), 1.154701, tolerance = 1e-6)
})

test_that("MI works for rnorm(50) and k=1", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(MI(X, k=1), 0.9743499, tolerance = 1e-6)
})

test_that("MI works for rnorm(50) and k=2", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(MI(X, k=2), 1.037983, tolerance = 1e-6)
})
