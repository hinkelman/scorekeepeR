
#' Initialize players table
#'
#' Initialize players table with no data
#'
#' @md
#'
#' @export
#'

init_players_table <- function(){
  data.frame(PlayerID = ids::random_id(),
             FirstName = NA_character_,
             LastName = NA_character_)
}


#' Add players row
#'
#' Add new row to players table
#'
#' @md
#' @param players_table  Players table
#'
#' @export
#'

add_players_row = function(players_table){
  rbind(players_table, init_players_table())
}


#' Edit players row
#'
#' Edit row in players table
#'
#' @md
#' @param players_table Players table
#' @param row           Row index of cell to update
#' @param column        Column index of cell to update
#' @param value         Value to place in cell
#'
#' @export
#'

edit_players_row = function(players_table, row, col, value){
  if (col == 1) stop("PlayerID column (col = 1) can't be updated")
  if (!is.character(value)) value = as.character(value)
  players_table[row, col] = value
  players_table
}
