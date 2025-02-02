
#' Initialize game stats table
#'
#' Initialize game stats table with no data. Only includes columns that are tallied, not calculated.
#'
#' @md
#'
#' @export
#'

init_game_stats_table <- function(){
  tmp = data.frame(PlayerID = character(),
                   GameID = character(),
                   DNP = integer())
  for (i in events) tmp[[i]] = integer()
  tmp
}

#' Add game stats
#'
#' Add new game (with no data) to game_stats_table
#'
#' @md
#' @param game_stats_table    Game stats table
#' @param player_ids          Vector of unique identifiers of players from the players table
#' @param game_id             Unique identifier of a game from the games table
#'
#'
#' @export
#'

add_game_stats <- function(game_stats_table, player_ids, game_id){
  tmp = data.frame(PlayerID = player_ids,
                   GameID = game_id)
  for (i in c("DNP", events)) tmp[[i]] = 0L
  rbind(game_stats_table, tmp)
}


#' Increment game stat
#'
#' Increment game stat in game_stats_table
#'
#' @md
#' @param game_stats_table    Game stats table
#' @param player_id           Unique identifiers of a player from the players table
#' @param game_id             Unique identifier of a game from the games table
#' @param stat                Statistic (column) to be incremented
#'
#' @export
#'
#'

increment_game_stat <- function(games_stats_table, player_id, game_id,
                                stat = c("DNP", events)){
  stat = match.arg(stat)
  gst = games_stats_table
  ri = which(gst$PlayerID == player_id & gst$GameID == game_id)
  gst[ri, stat] = gst[ri, stat] + 1L
  gst
}

