
test_that("create_log_entry", {
  expect_equal(create_log_entry(FALSE, "FGM3", "Stephen Curry"),
               "Made 3-pt field goal by Stephen Curry")
  expect_equal(create_log_entry(TRUE, "FGM3", "Travis"),
               "UNDO Made 3-pt field goal by Travis")
  expect_error(create_log_entry("false", "FTA", "Test"))
  expect_error(create_log_entry(FALSE, "FG", "Test"))
})
