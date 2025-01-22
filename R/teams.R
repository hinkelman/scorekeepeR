
#' Initialize teams table
#'
#' Initialize teams table with no data
#'
#' @md
#'
#' @export
#'

init_teams_table <- function(){
  data.frame(TeamID = character(),
             Season = character(),
             League = character(),
             Team = character(),
             Coach = character())
}


#' Add teams row
#'
#' Add new row to teams table
#'
#' @md
#' @param teams_table  Teams table
#'
#' @export
#'

add_teams_row = function(teams_table){
  rbind(teams_table,
        data.frame(TeamID = ids::random_id(),
                   Season = "",
                   League = "",
                   Team = "",
                   Coach = ""))
}

#' Delete teams row
#'
#' Delete row from teams table and update rosters and players tables.
#'
#' @md
#' @param teams_table    Teams table
#' @param teams_row      Index of row to delete from teams table
#' @param players_table  Players table
#' @param rosters_table  Rosters table
#'
#' @return List of updated teams_table, players_table, and rosters_table.
#'
#' @export
#'

delete_teams_row = function(teams_table, teams_row, players_table = NULL, rosters_table = NULL){
  if (nrow(teams_table) == 0) stop("Can't delete row from empty teams_table")
  if (is.null(players_table)) players_table = init_players_table()
  if (is.null(rosters_table)) rosters_table = init_rosters_table()

  team_id = teams_table$TeamID[teams_row]
  new_teams_table = teams_table[-teams_row,]
  new_rosters_table = rosters_table[rosters_table$TeamID != team_id, ]
  new_players_table = players_table[players_table$PlayerID %in% new_rosters_table$PlayerID, ]

  list(teams_table = new_teams_table,
       players_table = new_players_table,
       rosters_table = new_rosters_table)
}

#' Edit teams row
#'
#' Edit row in teams table
#'
#' @md
#' @param teams_table Teams table
#' @param row         Row index of cell to update
#' @param column      Column index of cell to update
#' @param value       Value to place in cell
#'
#' @export
#'

edit_teams_row = function(teams_table, row, col, value){
  if (col == 1) stop("TeamID column (col = 1) can't be updated")
  if (!is.character(value)) value = as.character(value)
  teams_table[row, col] = value
  teams_table
}
