
#' Initialize rosters table
#'
#' Initialize rosters table with no data. The rosters file (note plural rosters)
#' is saved to file but a roster view is not.
#'
#' @md
#'
#' @export
#'

init_rosters_table <- function(){
  data.frame(TeamID = character(),
             PlayerID = character(),
             Number = character())
}


#' Create roster view
#'
#' Create roster view. A roster view is not saved to file.
#'
#' @md
#' @param team_id        Unique identifier of a team from the teams table
#' @param players_table  Players table
#' @param rosters_table  Rosters table
#'
#' @export
#'

create_roster_view <- function(team_id, players_table, rosters_table){
  rosters_table |>
    dplyr::filter(TeamID == team_id) |>
    dplyr::left_join(players_table, by = dplyr::join_by(PlayerID)) |>
    # maintain desired column order
    dplyr::select(TeamID, PlayerID, FirstName, LastName, Number)
}

#' Add roster row
#'
#' Add new row to roster view.
#'
#' @md
#' @param team_id        Unique identifier of a team from the teams table
#' @param players_table  Players table
#' @param rosters_table  Rosters table
#'
#' @return List of updated players_table, rosters_table, and roster_view.
#'
#' @export
#'

add_roster_row = function(team_id, players_table, rosters_table){
  new_players_table = add_players_row(players_table)
  new_player_id = new_players_table$PlayerID[nrow(new_players_table)]

  new_rosters_table = rbind(rosters_table,
                            data.frame(TeamID = team_id,
                                       PlayerID = new_player_id,
                                       Number = NA_character_))

  new_roster_view = create_roster_view(team_id, new_players_table, new_rosters_table)

  list(players_table = new_players_table,
       rosters_table = new_rosters_table,
       roster_view = new_roster_view)
}


#' Delete roster row
#'
#' Delete row from roster view and update rosters and players tables.
#'
#' @md
#' @param roster_view    Roster view
#' @param roster_row     Index of row to delete from roster view
#' @param players_table  Players table
#' @param rosters_table  Rosters table
#'
#' @return List of updated players_table, rosters_table, and roster_view.
#'
#' @export
#'

delete_roster_row = function(roster_view, roster_row, players_table, rosters_table){
  if (nrow(roster_view) == 0) stop("Can't delete row from empty roster_view")
  team_id = roster_view$TeamID[roster_row]
  player_id = roster_view$PlayerID[roster_row]

  new_rosters_table = rosters_table[!(rosters_table$TeamID == team_id &
                                        rosters_table$PlayerID == player_id), ]
  new_players_table = players_table[players_table$PlayerID %in% new_rosters_table$PlayerID, ]
  new_roster_view = create_roster_view(team_id, new_players_table, new_rosters_table)

  list(players_table = new_players_table,
       rosters_table = new_rosters_table,
       roster_view = new_roster_view)
}

#' Edit roster row
#'
#' Edit row in roster view
#'
#' @md
#' @param roster_view    Roster view
#' @param roster_row     Row index of cell to update
#' @param roster_column  Column index of cell to update
#' @param value          Value to place in cell
#' @param players_table  Players table
#' @param rosters_table  Rosters table
#'
#' @return List of updated players_table, rosters_table, and roster_view.
#'
#' @export
#'

edit_roster_row = function(roster_view, roster_row, roster_col, value, players_table, rosters_table){
  # don't use row and col as indices b/c updating underlying tables, not the view directly

  if (roster_col == 1) stop("TeamID column (roster_col = 1) can't be updated")
  if (roster_col == 2) stop("PlayerID column (roster_col = 2) can't be updated")

  if (!is.character(value)) value = as.character(value)
  # check if value has any number of whitespace
  if (grepl("^\\s*$", value)) value = NA_character_

  team_id = roster_view$TeamID[roster_row]
  player_id = roster_view$PlayerID[roster_row]
  col_name = colnames(roster_view)[roster_col]

  new_players_table = players_table
  if (col_name %in% colnames(players_table)){
    new_players_table[new_players_table$PlayerID == player_id, col_name] = value
  }

  new_rosters_table = rosters_table
  if (col_name %in% colnames(rosters_table)){
    new_rosters_table[new_rosters_table$TeamID == team_id &
                        new_rosters_table$PlayerID == player_id, col_name] = value
  }

  new_roster_view = create_roster_view(team_id, new_players_table, new_rosters_table)

  list(players_table = new_players_table,
       rosters_table = new_rosters_table,
       roster_view = new_roster_view)
}
