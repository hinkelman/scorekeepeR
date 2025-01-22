teams_table = data.frame(TeamID = as.character(1:3),
                         Season = c("1st", "2nd", "3rd"),
                         League = "40+",
                         Team = "Knuckleheads",
                         Coach = "Smith")

test_that("teams table", {
  expect_equal(nrow(add_teams_row(teams_table)), 4)
  expect_true(all.equal(delete_teams_row(teams_table, 2, init_players_table(),
                                         init_rosters_table())$teams_table,
                        data.frame(TeamID = c("1", "3"),
                                   Season = c("1st", "3rd"),
                                   League = "40+",
                                   Team = "Knuckleheads",
                                   Coach = "Smith"),
                        check.attributes = FALSE))
  expect_length(delete_teams_row(teams_table, 2, init_players_table(), init_rosters_table()), 3)
  expect_error(delete_teams_row(init_teams_table(), 1, init_players_table(), init_rosters_table()))
  expect_error(edit_teams_row(teams_table, 1, 1, "test"))
  expect_true(all.equal(edit_teams_row(teams_table, 1, 2, "First"),
                        data.frame(TeamID = as.character(1:3),
                                   Season = c("First", "2nd", "3rd"),
                                   League = "40+",
                                   Team = "Knuckleheads",
                                   Coach = "Smith"),
                        check.attributes = FALSE))
})
