
# ---- init_game_stats_table ----

test_that("init_game_stats_table returns empty table with expected columns", {
  gst = init_game_stats_table()
  expect_equal(nrow(gst), 0)
  expect_true("PlayerID" %in% colnames(gst))
  expect_true("GameID" %in% colnames(gst))
  expect_true("DNP" %in% colnames(gst))
  # all events should be present as columns
  for (e in events) expect_true(e %in% colnames(gst))
})

# ---- add_game_stats ----

test_that("add_game_stats adds correct rows", {
  gst = init_game_stats_table()
  player_ids = c("p1", "p2", "p3")
  gst = add_game_stats(gst, player_ids, "g1")
  expect_equal(nrow(gst), 3)
  expect_equal(gst$PlayerID, player_ids)
  expect_true(all(gst$GameID == "g1"))
  # all stat columns should be 0
  expect_true(all(gst$DNP == 0L))
  for (e in events) expect_true(all(gst[[e]] == 0L))
})

test_that("add_game_stats appends to existing table", {
  gst = init_game_stats_table()
  gst = add_game_stats(gst, c("p1", "p2"), "g1")
  gst = add_game_stats(gst, c("p1", "p2"), "g2")
  expect_equal(nrow(gst), 4)
  expect_equal(sum(gst$GameID == "g1"), 2)
  expect_equal(sum(gst$GameID == "g2"), 2)
})

# ---- update_game_stat ----

test_that("update_game_stat increments and decrements", {
  gst = init_game_stats_table()
  gst = add_game_stats(gst, c("p1", "p2"), "g1")
  stat = events[1]
  gst = update_game_stat(gst, "p1", "g1", stat, undo = FALSE)
  expect_equal(gst[gst$PlayerID == "p1", stat], 1L)
  expect_equal(gst[gst$PlayerID == "p2", stat], 0L)
  gst = update_game_stat(gst, "p1", "g1", stat, undo = TRUE)
  expect_equal(gst[gst$PlayerID == "p1", stat], 0L)
})

test_that("update_game_stat errors on bad undo", {
  gst = add_game_stats(init_game_stats_table(), "p1", "g1")
  expect_error(update_game_stat(gst, "p1", "g1", events[1], undo = "false"))
})

test_that("update_game_stat errors on invalid stat", {
  gst = add_game_stats(init_game_stats_table(), "p1", "g1")
  expect_error(update_game_stat(gst, "p1", "g1", "INVALID"))
})

# ---- update_dnp ----

test_that("update_dnp sets DNP correctly", {
  gst = init_game_stats_table()
  gst = add_game_stats(gst, c("p1", "p2", "p3"), "g1")
  gst = update_dnp(gst, c("p1", "p3"), "g1")
  expect_equal(gst$DNP[gst$PlayerID == "p1"], 1)
  expect_equal(gst$DNP[gst$PlayerID == "p2"], 0)
  expect_equal(gst$DNP[gst$PlayerID == "p3"], 1)
})

test_that("update_dnp resets previous DNP values", {
  gst = init_game_stats_table()
  gst = add_game_stats(gst, c("p1", "p2"), "g1")
  gst = update_dnp(gst, "p1", "g1")
  expect_equal(gst$DNP[gst$PlayerID == "p1"], 1)
  # now change DNP to p2 only
  gst = update_dnp(gst, "p2", "g1")
  expect_equal(gst$DNP[gst$PlayerID == "p1"], 0)
  expect_equal(gst$DNP[gst$PlayerID == "p2"], 1)
})

test_that("update_dnp with NULL player_ids clears all DNP", {
  gst = init_game_stats_table()
  gst = add_game_stats(gst, c("p1", "p2"), "g1")
  gst = update_dnp(gst, "p1", "g1")
  gst = update_dnp(gst, NULL, "g1")
  expect_true(all(gst$DNP == 0))
})

test_that("update_dnp only affects specified game", {
  gst = init_game_stats_table()
  gst = add_game_stats(gst, c("p1", "p2"), "g1")
  gst = add_game_stats(gst, c("p1", "p2"), "g2")
  gst = update_dnp(gst, "p1", "g1")
  expect_equal(gst$DNP[gst$PlayerID == "p1" & gst$GameID == "g1"], 1)
  expect_equal(gst$DNP[gst$PlayerID == "p1" & gst$GameID == "g2"], 0)
})

test_that("update_dnp keeps DNP as integer", {
  gst = add_game_stats(init_game_stats_table(), c("p1", "p2"), "g1")
  gst = update_dnp(gst, "p1", "g1")
  expect_type(gst$DNP, "integer")
})
