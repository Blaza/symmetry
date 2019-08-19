context("MK")

test_that("MK works for (1, 2, 3) and k=1", {
  expect_equal(MK(1:3, k=1), 1.732051, tolerance = 1e-6)
})

test_that("MK works for (1, 2, 3, 4) and k=1", {
  expect_equal(MK(1:4, 1), 2)
})

test_that("MK works for rnorm(50) and k=1", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(MK(X, 1), 2.174714, tolerance = 1e-6)
})

test_that("MK works for rnorm(50) and k=2", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(MK(X, 2), 2.530928, tolerance = 1e-6)
})
