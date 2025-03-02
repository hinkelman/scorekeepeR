
test_that("points", {
  expect_equal(calc_points(10, 5, 3), 29)
  expect_equal(calc_points(0, 3, 8), 30)
  expect_equal(calc_points(c(10, 0), c(5, 3), c(3, 8)), c(29, 30))
  expect_error(calc_points("2", 1, 1))
})

test_that("shooting", {
  expect_equal(calc_shooting(1, 2), 50)
  expect_equal(calc_shooting(2, 3), 67)
  expect_equal(calc_shooting(1:2, 2:3), c(50, 67))
  expect_equal(calc_shooting(0, 0), 0)
  expect_error(calc_shooting(10, 3))
  expect_error(calc_shooting(2, 1))
  expect_error(calc_shooting(1:2, c(1, 1)))
  expect_error(calc_shooting("1", 2))
})

test_that("true shooting", {
  expect_equal(calc_true_shooting(0, 10, 10), 0)
  expect_equal(calc_true_shooting(3, 0, 1), 150)
  expect_equal(calc_true_shooting(88, 100, 0), 100)
  expect_equal(calc_true_shooting(20, 0, 20), 50)
  expect_equal(calc_true_shooting(c(0, 3, 88, 20),
                                 c(10, 0, 100, 0),
                                 c(10, 1, 0, 20)),
              c(0, 150, 100, 50))
  expect_equal(calc_true_shooting(10, 0, 0), 0)
  expect_error(calc_true_shooting(4, 0, 1))
  expect_error(calc_true_shooting(c(4, 10, 2), rep(0, 3), c(1, 0, 10)))
  expect_error(calc_true_shooting("10", "10", "10"))
})

test_that("calc_efficiency", {
  expect_equal(calc_efficiency(15, 9, 3, 1, 0, 14, 6, 4, 3, 1), 18)
  expect_equal(calc_efficiency(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 0)
  expect_error(calc_efficiency("15", 9, 3, 1, 0, 14, 6, 4, 3, 1))
})
