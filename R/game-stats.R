
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


#' Update game stat
#'
#' Update game stat in game_stats_table
#'
#' @md
#' @param game_stats_table    Game stats table
#' @param player_id           Unique identifier of a player from the players table
#' @param game_id             Unique identifier of a game from the games table
#' @param stat                Statistic (column) to be changed
#' @param undo                When true, stat is decremented.
#'
#' @export
#'
#'

update_game_stat <- function(games_stats_table, player_id, game_id,
                             stat = events, undo = FALSE){
  if (!is.logical(undo)) stop("undo must be TRUE or FALSE")
  stat = match.arg(stat)
  gst = games_stats_table
  ri = which(gst$PlayerID == player_id & gst$GameID == game_id)
  change = if (undo) -1L else 1L
  gst[ri, stat] = gst[ri, stat] + change
  gst
}


#' Update DNP
#'
#' Update DNP (did not play) in game_stats_table
#'
#' @md
#' @param game_stats_table    Game stats table
#' @param player_ids          Unique identifiers of a player from the players table
#' @param game_id             Unique identifier of a game from the games table
#'
#' @export
#'
#'

update_dnp <- function(games_stats_table, player_ids, game_id){
  gst = games_stats_table
  ri_all = which(gst$GameID == game_id)
  gst$DNP[ri_all] = 0
  if (!is.null(player_ids)){
    ri_dnp = which(gst$PlayerID %in% player_ids & gst$GameID == game_id)
    gst$DNP[ri_dnp] = 1
  }
  gst
}

