players_table = data.frame(PlayerID = as.character(1:3),
                           FirstName = c("Travis", "Jared", "Owen"),
                           LastName = "Hinkelman")

pt2 = add_players_row(players_table)

test_that("players", {
  expect_equal(nrow(pt2), 4)
  expect_equal(pt2$FirstName[4], NA_character_)
  expect_length(create_player_names(NA, NA, NA, "roster"), 0)
  expect_length(create_player_names(NA, NA, "32", "roster"), 0)
  expect_length(create_player_names(NA, NA, NA, "scorekeeper"), 0)
  expect_equal(create_player_names(NA, "Curry", NA, "roster"), "Curry")
  expect_equal(create_player_names("Steph", NA, NA, "roster"), "Steph")
  expect_equal(create_player_names("Steph", "Curry", NA, "roster"), "Steph Curry")
  expect_equal(create_player_names("Steph", "Curry", "32", "roster"), "Steph Curry")
  expect_equal(create_player_names(NA, NA, "32", "scorekeeper"), "#32")
  expect_equal(create_player_names(NA, "Curry", NA, "scorekeeper"), "Curry")
  expect_equal(create_player_names("Steph", NA, NA, "scorekeeper"), "Steph")
  expect_equal(create_player_names("Steph", "Curry", NA, "scorekeeper"), "Steph")
  expect_equal(create_player_names("Steph", "Curry", "32", "scorekeeper"), "Steph (#32)")
})
