
players_table = data.frame(PlayerID = c("1", "2"),
                           FirstName = c("Travis", "Jared"),
                           LastName = c(NA, NA))
pt2 = add_players_row(players_table)

tmp = data.frame(FirstName = c("", NA, "Steph", NA, "Steph", "Steph"),
                 LastName = c("Hink", "Curry", NA, NA, "Curry", "Curry"),
                 Number = c(" ", NA, NA, "32", NA, "32"))

test_that("players", {
  expect_equal(nrow(pt2), 3)
  expect_equal(pt2$FirstName[3], NA_character_)
  expect_true(all.equal(dplyr::mutate(tmp,
                                      Name = create_player_name(FirstName, LastName),
                                      NameNum = create_player_namenum(FirstName, LastName, Number)),
                        cbind(tmp,
                              data.frame(Name = c("Hink", "Curry", "Steph", NA,
                                                  "Steph Curry", "Steph Curry"),
                                         NameNum = c("Hink", "Curry", "Steph", "#32",
                                                     "Steph", "Steph (#32)")))))
})
