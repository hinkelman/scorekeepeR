
#' Initialize players table
#'
#' Initialize players table with no data
#'
#' @md
#'
#' @export
#'

init_players_table <- function(){
  data.frame(PlayerID = character(),
             FirstName = character(),
             LastName = character())
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
  rbind(players_table,
        data.frame(PlayerID = ids::random_id(),
                   FirstName = NA_character_,
                   LastName = NA_character_))
}

