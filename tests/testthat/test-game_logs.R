
test_that("add_log_entry", {
  expect_equal(add_log_entry(NULL, "Stephen Curry", "FGM3", FALSE),
               "Made 3-pt field goal by Stephen Curry")
  expect_equal(add_log_entry(NULL, "Travis", "FGM3", TRUE),
               "UNDO Made 3-pt field goal by Travis")
  expect_error(add_log_entry(NULL, "Test", "FGM3", "false"))
  expect_error(add_log_entry(NULL, "Test", "FG", FALSE))
})
