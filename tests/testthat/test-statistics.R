
test_that("true shooting", {
  expect_equal(true_shooting(0, 10, 10), 0)
  expect_equal(true_shooting(3, 0, 1), 150)
  expect_equal(true_shooting(88, 100, 0), 100)
  expect_equal(true_shooting(20, 0, 20), 50)
  expect_error(true_shooting(10, 0, 0))
  expect_error(true_shooting(4, 0, 1))
  expect_error(true_shooting("10", "10", "10"))
})
