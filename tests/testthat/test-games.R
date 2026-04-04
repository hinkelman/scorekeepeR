
# ---- init_games_table ----

test_that("init_games_table returns empty table with expected columns", {
  gt = init_games_table()
  expect_equal(nrow(gt), 0)
  expect_equal(colnames(gt), c("TeamID", "GameID", "Date", "Opponent",
                                "TeamScore", "OpponentScore"))
})

# ---- update_games_row ----

test_that("update_games_row inserts new row", {
  gt = init_games_table()
  gt = update_games_row(gt, "t1", "g1", "2024-01-15", "Sharks", 55, 48)
  expect_equal(nrow(gt), 1)
  expect_equal(gt$TeamID, "t1")
  expect_equal(gt$GameID, "g1")
  expect_equal(gt$TeamScore, 55)
  expect_equal(gt$OpponentScore, 48)
})

test_that("update_games_row updates existing row", {
  gt = init_games_table()
  gt = update_games_row(gt, "t1", "g1", "2024-01-15", "Sharks", 55, 48)
  gt = update_games_row(gt, "t1", "g1", "2024-01-15", "Sharks", 60, 52)
  expect_equal(nrow(gt), 1)
  expect_equal(gt$TeamScore, 60)
  expect_equal(gt$OpponentScore, 52)
})

test_that("update_games_row adds multiple games", {
  gt = init_games_table()
  gt = update_games_row(gt, "t1", "g1", "2024-01-15", "Sharks", 55, 48)
  gt = update_games_row(gt, "t1", "g2", "2024-01-22", "Eagles", 42, 50)
  expect_equal(nrow(gt), 2)
  expect_equal(gt$GameID, c("g1", "g2"))
})

test_that("update_games_row converts scores to numeric", {
  gt = init_games_table()
  gt = update_games_row(gt, "t1", "g1", "2024-01-15", "Sharks", "55", "48")
  expect_true(is.numeric(gt$TeamScore))
  expect_true(is.numeric(gt$OpponentScore))
})
