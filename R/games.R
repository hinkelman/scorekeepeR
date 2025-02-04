
#' Initialize games table
#'
#' Initialize games table with no data.
#'
#' @md
#'
#' @export
#'

init_games_table <- function(){
  data.frame(TeamID = character(),
             GameID = character(),
             Date = character(),
             Opponent = character(),
             TeamScore = character(),
             OpponentScore = character())
}


#' Update games row
#'
#' Add or update row in games table
#'
#' @md
#' @param games_table    Games table
#' @param team_id        Unique identifier of a team from the teams table
#' @param game_id        Unique identifier of a game from the games table
#' @param date           Date game was played
#' @param opponent       Opponent of focal team
#' @param team_score     Final score for focal team
#' @param opponent_score Final score for opponent
#'
#'
#' @export
#'

update_games_row = function(games_table, team_id, game_id, date, opponent,
                            team_score, opponent_score){
  ri = if (game_id %in% games_table$GameID) which(games_table$GameID == game_id) else nrow(games_table) + 1
  games_table[ri,] = list(team_id, game_id, date, opponent, team_score,
                          opponent_score)
  games_table
}

