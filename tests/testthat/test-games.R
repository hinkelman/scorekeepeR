
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

test_that("update_games_row preserves column types", {
  gt = update_games_row(init_games_table(), "t1", "g1", as.Date("2024-01-15"),
                        "Sharks", "55", 48)
  expect_equal(sapply(gt, class), sapply(init_games_table(), class))
  expect_equal(gt$Date, "2024-01-15")
})

test_that("update_games_row updates all fields of existing row", {
  gt = update_games_row(init_games_table(), "t1", "g1", "2024-01-15", "Sharks", 55, 48)
  gt = update_games_row(gt, "t2", "g1", "2024-01-16", "Eagles", 60, 52)
  expect_equal(nrow(gt), 1)
  expect_equal(unlist(gt[1, ], use.names = FALSE),
               c("t2", "g1", "2024-01-16", "Eagles", "60", "52"))
})
