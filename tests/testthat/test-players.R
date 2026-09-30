
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

test_that("init_players_table returns empty table with expected columns", {
  pt = init_players_table()
  expect_equal(nrow(pt), 0)
  expect_equal(colnames(pt), c("PlayerID", "FirstName", "LastName"))
})

test_that("add_players_row creates unique ID with NA names", {
  pt = add_players_row(add_players_row(init_players_table()))
  expect_equal(nrow(pt), 2)
  expect_false(any(is.na(pt$PlayerID)))
  expect_false(pt$PlayerID[1] == pt$PlayerID[2])
  expect_true(all(is.na(pt$FirstName)))
  expect_true(all(is.na(pt$LastName)))
})

test_that("replace_space converts blank strings to NA", {
  expect_equal(replace_space(c("", "   ", "\t", NA, "Steph", " Steph ")),
               c(NA, NA, NA, NA, "Steph", " Steph "))
  expect_equal(replace_space(c(1, NA)), c(1, NA))
})
