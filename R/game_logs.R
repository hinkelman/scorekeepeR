
#' Create log entry
#'
#' Create entry for appending to game log
#'
#' @md
#' @param undo_bool  Boolean indicating if event was undone
#' @param event      Game event (e.g., FGM, STL, etc.)
#' @param player     Player name
#'
#' @export
#' @examples
#' create_log_entry(FALSE, "FGM3", "Stephen Curry")
#'

create_log_entry <- function(undo_bool, event = events, player){
  event = match.arg(event)
  undo = if(undo_bool) "UNDO " else ""
  paste0(undo, events_desc[events == event], " by ", player)
}
