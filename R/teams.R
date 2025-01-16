
#' Initialize teams table
#'
#' Initialize teams table with no data
#'
#' @md
#'
#' @export
#'

init_teams_table <- function(){
  data.frame(TeamID = ids::random_id(),
             Season = NA_character_,
             League = NA_character_,
             Team = NA_character_,
             Coach = NA_character_)
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
  rbind(teams_table, init_teams_table())
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
