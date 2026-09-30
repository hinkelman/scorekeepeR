
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
  expect_equal(calc_shooting(0, 0), NA_real_)
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
  expect_equal(calc_true_shooting(10, 0, 0), NA_real_)
  expect_error(calc_true_shooting("10", "10", "10"))
})

test_that("calc_efficiency", {
  expect_equal(calc_efficiency(15, 9, 3, 1, 0, 14, 6, 4, 3, 1), 18)
  expect_equal(calc_efficiency(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 0)
  expect_error(calc_efficiency("15", 9, 3, 1, 0, 14, 6, 4, 3, 1))
})

test_that("calc_shooting warns when made > attempted", {
  # two warnings emitted
  expect_warning(expect_warning(calc_shooting(5, 3)))
})

test_that("calc_true_shooting warns when TS% > 150", {
  # PTS = 10, FTA = 0, FGA = 1 -> TS% = 10/(2*1)*100 = 500
  expect_warning(calc_true_shooting(10, 0, 1))
})

test_that("calc_game_stats computes all derived columns", {
  data = data.frame(FTM = 3, FTA = 5, FGM2 = 4, FGA2 = 8,
                    FGM3 = 2, FGA3 = 5, OREB = 3, DREB = 4,
                    AST = 5, STL = 2, BLK = 1, TOV = 3)
  result = calc_game_stats(data)
  expect_equal(result$PTS, 3 + 4*2 + 2*3)  # 17
  expect_equal(result$REB, 7)
  expect_equal(result$FGM, 6)
  expect_equal(result$FGA, 13)
  expect_true("FT%" %in% colnames(result))
  expect_true("FG%" %in% colnames(result))
  expect_true("3P%" %in% colnames(result))
  expect_true("TS%" %in% colnames(result))
  expect_true("EFF" %in% colnames(result))
})

test_that("calc_shooting handles NA", {
  expect_equal(calc_shooting(c(1, NA), c(2, 2)), c(50, NA))
  expect_equal(calc_shooting(1, NA), NA_real_)
})

test_that("calc_game_stats computes correct derived values", {
  data = data.frame(FTM = 3, FTA = 5, FGM2 = 4, FGA2 = 8,
                    FGM3 = 2, FGA3 = 5, OREB = 3, DREB = 4,
                    AST = 5, STL = 2, BLK = 1, TOV = 3)
  result = calc_game_stats(data)
  expect_equal(result$`FT%`, 60)  # 3/5
  expect_equal(result$`FG%`, 46)  # 6/13
  expect_equal(result$`3P%`, 40)  # 2/5
  expect_equal(result$`TS%`, 56)  # 17/(0.88*5 + 2*13) = 55.9
  expect_equal(result$EFF, 20)    # 17+7+5+2+1 - 7 - 2 - 3
})

test_that("calc_game_stats handles multiple rows and zero attempts", {
  gst = add_game_stats(init_game_stats_table(), c("p1", "p2"), "g1")
  gst = update_game_stat(gst, "p1", "g1", "FGM2")
  gst = update_game_stat(gst, "p1", "g1", "FGA2")
  # missed shot only increments attempts
  gst = update_game_stat(gst, "p1", "g1", "FGA2")
  result = calc_game_stats(gst)
  expect_equal(nrow(result), 2)
  expect_equal(result$PTS, c(2, 0))
  expect_equal(result$`FG%`, c(50, NA))
  # no free throws or threes attempted
  expect_true(all(is.na(result$`FT%`)))
  expect_true(all(is.na(result$`3P%`)))
  # p2 has no attempts at all
  expect_true(is.na(result$`TS%`[2]))
})
