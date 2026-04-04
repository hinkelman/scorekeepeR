
# ---- setup ----

players_table = data.frame(PlayerID = c("p1", "p2", "p3"),
                           FirstName = c("Alice", "Bob", "Carol"),
                           LastName = c("Smith", "Jones", "Lee"))

rosters_table = data.frame(TeamID = c("t1", "t1", "t2"),
                           PlayerID = c("p1", "p2", "p3"),
                           Number = c("10", "23", "5"))

# ---- init_rosters_table ----

test_that("init_rosters_table returns empty table with expected columns", {
  rt = init_rosters_table()
  expect_equal(nrow(rt), 0)
  expect_equal(colnames(rt), c("TeamID", "PlayerID", "Number"))
})

# ---- create_roster_view ----

test_that("create_roster_view filters by team and joins player info", {
  rv = create_roster_view("t1", players_table, rosters_table)
  expect_equal(nrow(rv), 2)
  expect_equal(rv$PlayerID, c("p1", "p2"))
  expect_equal(colnames(rv), c("TeamID", "PlayerID", "FirstName", "LastName", "Number"))
})

test_that("create_roster_view returns empty for unknown team", {
  rv = create_roster_view("t_unknown", players_table, rosters_table)
  expect_equal(nrow(rv), 0)
})

# ---- add_roster_row ----

test_that("add_roster_row adds row to all tables", {
  result = add_roster_row("t1", players_table, rosters_table)
  expect_equal(nrow(result$players_table), nrow(players_table) + 1)
  expect_equal(nrow(result$rosters_table), nrow(rosters_table) + 1)
  expect_equal(nrow(result$roster_view), 3)
  # new player should have NA names
  new_player = result$players_table[nrow(result$players_table), ]
  expect_true(is.na(new_player$FirstName))
  expect_true(is.na(new_player$LastName))
})

# ---- delete_roster_row ----

test_that("delete_roster_row removes player from roster and players table", {
  rv = create_roster_view("t1", players_table, rosters_table)
  result = delete_roster_row(rv, 1, players_table, rosters_table)
  expect_equal(nrow(result$roster_view), 1)
  expect_false("p1" %in% result$rosters_table$PlayerID[result$rosters_table$TeamID == "t1"])
  # p1 not on any other team, so removed from players table too
  expect_false("p1" %in% result$players_table$PlayerID)
})

test_that("delete_roster_row keeps player if on another team", {
  # put p1 on both teams
  rosters_both = rbind(rosters_table,
                       data.frame(TeamID = "t2", PlayerID = "p1", Number = "99"))
  rv = create_roster_view("t1", players_table, rosters_both)
  result = delete_roster_row(rv, 1, players_table, rosters_both)
  # p1 removed from t1 roster but still in players table (on t2)
  expect_false("p1" %in% result$rosters_table$PlayerID[result$rosters_table$TeamID == "t1"])
  expect_true("p1" %in% result$players_table$PlayerID)
})

test_that("delete_roster_row errors on empty roster", {
  rv = create_roster_view("t_unknown", players_table, rosters_table)
  expect_error(delete_roster_row(rv, 1, players_table, rosters_table))
})

# ---- edit_roster_row ----

test_that("edit_roster_row updates first name in players table", {
  rv = create_roster_view("t1", players_table, rosters_table)
  # col 3 = FirstName
  result = edit_roster_row(rv, 1, 3, "Alicia", players_table, rosters_table)
  expect_equal(result$players_table$FirstName[result$players_table$PlayerID == "p1"], "Alicia")
})

test_that("edit_roster_row updates number in rosters table", {
  rv = create_roster_view("t1", players_table, rosters_table)
  # col 5 = Number
  result = edit_roster_row(rv, 1, 5, "99", players_table, rosters_table)
  expect_equal(result$rosters_table$Number[result$rosters_table$PlayerID == "p1" &
                                             result$rosters_table$TeamID == "t1"], "99")
})

test_that("edit_roster_row converts whitespace to NA", {
  rv = create_roster_view("t1", players_table, rosters_table)
  result = edit_roster_row(rv, 1, 3, "   ", players_table, rosters_table)
  expect_true(is.na(result$players_table$FirstName[result$players_table$PlayerID == "p1"]))
})

test_that("edit_roster_row errors on protected columns", {
  rv = create_roster_view("t1", players_table, rosters_table)
  expect_error(edit_roster_row(rv, 1, 1, "new_team", players_table, rosters_table))
  expect_error(edit_roster_row(rv, 1, 2, "new_player", players_table, rosters_table))
})
