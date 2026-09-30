
test_that("add_log_entry", {
  expect_equal(add_log_entry(NULL, "Stephen Curry", "FGM3", FALSE),
               "Made 3-pt field goal by Stephen Curry")
  expect_equal(add_log_entry(NULL, "Travis", "FGM3", TRUE),
               "UNDO Made 3-pt field goal by Travis")
  expect_error(add_log_entry(NULL, "Test", "FGM3", "false"))
  expect_error(add_log_entry(NULL, "Test", "FG", FALSE))
})

test_that("create_log_header returns expected format", {
  header = create_log_header("t1", "g1", "2024-01-15", "Sharks")
  expect_length(header, 5)
  expect_true(grepl("^Date:", header[1]))
  expect_true(grepl("^Opponent:", header[2]))
  expect_true(grepl("^TeamID:", header[3]))
  expect_true(grepl("^GameID:", header[4]))
  expect_true(grepl("^-+$", header[5]))
})

test_that("add_log_entry appends to existing log", {
  log = create_log_header("t1", "g1", "2024-01-15", "Sharks")
  log = add_log_entry(log, "Alice", events[1], FALSE)
  expect_length(log, 6)
})

test_that("add_log_entry maps every event to its description", {
  expected = c(FTM = "Made free throw", FTA = "Missed free throw",
               FGM2 = "Made field goal", FGA2 = "Missed field goal",
               FGM3 = "Made 3-pt field goal", FGA3 = "Missed 3-pt field goal",
               STL = "Steal", TOV = "Turnover", DREB = "Defensive Rebound",
               OREB = "Offensive Rebound", BLK = "Block", AST = "Assist", PF = "Foul")
  expect_equal(sort(names(expected)), sort(events))
  for (e in events) {
    expect_equal(add_log_entry(NULL, "Alice", e), paste(expected[[e]], "by Alice"))
  }
})

test_that("add_log_entry errors on NA or vector undo", {
  expect_error(add_log_entry(NULL, "Test", "FGM3", NA), "undo must be TRUE or FALSE")
  expect_error(add_log_entry(NULL, "Test", "FGM3", c(TRUE, FALSE)), "undo must be TRUE or FALSE")
})

test_that("create_log_header contains full values", {
  header = create_log_header("t1", "g1", "2024-01-15", "Sharks")
  expect_equal(header[1:4], c("Date: 2024-01-15", "Opponent: Sharks",
                              "TeamID: t1", "GameID: g1"))
})
