
test_that("events and events_desc are aligned", {
  expect_equal(length(events), length(events_desc))
  expect_false(anyDuplicated(events) > 0)
  expect_false(anyDuplicated(events_desc) > 0)
})

test_that("display and stats columns are produced by calc_game_stats", {
  gst = add_game_stats(init_game_stats_table(), "p1", "g1")
  out = calc_game_stats(gst)
  expect_equal(setdiff(stats_display_cols, colnames(out)), character(0))
  expect_equal(setdiff(stats_cols, colnames(out)), character(0))
})
