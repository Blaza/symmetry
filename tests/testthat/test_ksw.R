context("KSW")

test_that("KS works for (1, 2, 3)", {
  expect_equal(KS(1:3), 1)
})

test_that("SGN works for (1, 2, 3)", {
  expect_equal(SGN(1:3), 1/2)
})

test_that("SGN works for (-1, 0, 1)", {
  expect_equal(SGN(-1:1), -1/6)
})

test_that("WCX works for (1, 2, 3)", {
  expect_equal(WCX(1:3), 1/2)
})

test_that("WCX works for (-1, 0, 1)", {
  expect_equal(WCX(-1:1), -1/6)
})

