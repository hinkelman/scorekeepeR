
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
