test_that("I1 works for (1, 2, 3) and k=1", {
  expect_equal(I1(1:3, 1), 4/9)
})

test_that("I1 works for rnorm(50) and k=1", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(I1(X, 1), 0.07266939, tolerance = 1e-8)
})

test_that("I1 works for rnorm(50) and k=2", {
  set.seed(1)
  X <- rnorm(50)
  expect_equal(I1(X, 2), 0.08693539, tolerance = 1e-8)
})
