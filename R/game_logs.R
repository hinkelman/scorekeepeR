
#' Create log header
#'
#' Create log header for top of game log
#'
#' @md
#' @param team_id        Unique identifier of a team from the teams table
#' @param game_id        Unique identifier of a game from the games table
#' @param date           Date game was played
#' @param opponent       Opponent of focal team
#'
#' @export


create_log_header <- function(team_id, game_id, date, opponent){
  c(paste0("GameID: ", game_id, "; TeamID: ", team_id,
           "; Date: ", date, "; Opponent: ", opponent),
    "---------------------------------------------------------------------")
}

#' Add log entry
#'
#' Create and append entry to game log
#'
#' @md
#' @param game_log   Game log comprised of vector of event descriptions
#' @param event      Game event (e.g., FGM, STL, etc.)
#' @param player     Player name
#' @param undo       Boolean indicating if event was undone
#'
#' @export
#'

add_log_entry <- function(game_log, event = events, player, undo = FALSE){
  if (!is.logical(undo)) stop("undo must be TRUE or FALSE")
  event = match.arg(event)
  undo_txt = if (undo) "UNDO " else ""
  c(game_log, paste0(undo_txt, events_desc[events == event], " by ", player))
}
