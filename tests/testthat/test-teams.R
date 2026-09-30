
test_that("teams table", {
  teams_table = data.frame(TeamID = as.character(1:3),
                         Season = c("1st", "2nd", "3rd"),
                         League = "40+",
                         Team = "Knuckleheads")
  expect_equal(nrow(add_teams_row(teams_table)), 4)
  expect_true(all.equal(delete_teams_row(teams_table, 2)$teams_table,
                        data.frame(TeamID = c("1", "3"),
                                   Season = c("1st", "3rd"),
                                   League = "40+",
                                   Team = "Knuckleheads"),
                        check.attributes = FALSE))
  expect_length(delete_teams_row(teams_table, 2), 3)
  expect_error(delete_teams_row(init_teams_table(), 1))
  expect_error(edit_teams_row(teams_table, 1, 1, "test"))
  expect_true(all.equal(edit_teams_row(teams_table, 1, 2, "First"),
                        data.frame(TeamID = as.character(1:3),
                                   Season = c("First", "2nd", "3rd"),
                                   League = "40+",
                                   Team = "Knuckleheads"),
                        check.attributes = FALSE))
})

test_that("edit_teams_row converts whitespace to NA", {
  tt = data.frame(TeamID = "1", Season = "1st",
                  League = "40+", Team = "Knuckleheads")
  result = edit_teams_row(tt, 1, 2, "  ")
  expect_true(is.na(result$Season[1]))
})

test_that("edit_teams_row validates row and col", {
  tt = data.frame(TeamID = c("1", "2"), Season = c("1st", "2nd"),
                  League = "40+", Team = "Knuckleheads")
  expect_equal(edit_teams_row(tt, 2L, 2L, "Second")$Season[2], "Second")
  expect_error(edit_teams_row(tt, 1, "Season", "x"), "numeric")
  expect_error(edit_teams_row(tt, 1, 5, "x"), "col must be between 2 and 4")
  expect_error(edit_teams_row(tt, 3, 2, "x"), "row must be between 1 and 2")
  expect_error(edit_teams_row(tt, 0, 2, "x"), "row must be between 1 and 2")
})

test_that("delete_teams_row validates teams_row", {
  tt = data.frame(TeamID = c("t1", "t2"), League = "L", Team = "T", Season = "S")
  rt = data.frame(TeamID = c("t1", "t2"), PlayerID = c("p1", "p2"), Number = c("1", "2"))
  pt = data.frame(PlayerID = c("p1", "p2"), FirstName = c("A", "B"), LastName = c("a", "b"))
  expect_error(delete_teams_row(tt, 0, pt, rt), "teams_row must be between 1 and 2")
  expect_error(delete_teams_row(tt, -1, pt, rt), "teams_row must be between 1 and 2")
  expect_error(delete_teams_row(tt, 3, pt, rt), "teams_row must be between 1 and 2")
  expect_error(delete_teams_row(tt, "1", pt, rt), "numeric")
})

test_that("delete_teams_row removes team's roster and orphaned players", {
  tt = data.frame(TeamID = c("t1", "t2"), League = "L", Team = "T", Season = "S")
  rt = data.frame(TeamID = c("t1", "t1", "t2"), PlayerID = c("p1", "p2", "p2"),
                  Number = c("1", "2", "2"))
  pt = data.frame(PlayerID = c("p1", "p2"), FirstName = c("A", "B"), LastName = c("a", "b"))
  result = delete_teams_row(tt, 1, pt, rt)
  expect_equal(result$teams_table$TeamID, "t2")
  expect_equal(result$rosters_table$TeamID, "t2")
  # p1 only on t1 so removed; p2 still on t2 so kept
  expect_equal(result$players_table$PlayerID, "p2")
})
