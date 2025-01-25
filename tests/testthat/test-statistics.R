
test_that("true shooting", {
  expect_equal(calc_true_shooting(0, 10, 10), 0)
  expect_equal(calc_true_shooting(3, 0, 1), 150)
  expect_equal(calc_true_shooting(88, 100, 0), 100)
  expect_equal(calc_true_shooting(20, 0, 20), 50)
  expect_error(calc_true_shooting(10, 0, 0))
  expect_error(calc_true_shooting(4, 0, 1))
  expect_error(calc_true_shooting("10", "10", "10"))
})

test_that("calc_efficiency", {
  expect_equal(calc_efficiency(15, 9, 3, 1, 0, 14, 6, 4, 3, 1), 18)
  expect_equal(calc_efficiency(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 0)
  expect_error(calc_efficiency("15", 9, 3, 1, 0, 14, 6, 4, 3, 1))
})
