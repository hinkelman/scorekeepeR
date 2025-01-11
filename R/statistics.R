
#' True shooting percentage
#'
#' Calculate true shooting percentage
#'
#' @md
#' @param PTS  Points
#' @param FTA  Free throws attempted
#' @param FGA  Field goals attempted
#'
#' @export
#' @examples
#' true_shooting(18, 4, 12)
#'

true_shooting <- function(PTS, FTA, FGA){
  # https://en.wikipedia.org/wiki/True_shooting_percentage
  ts = PTS/(0.88 * FTA + 2 * FGA) * 100
  if(ts > 150) stop("TS% exceeds maximum value (150%)")
  ts
}


#' Efficiency
#'
#' Calculate efficiency
#'
#' @md
#' @param PTS  Points
#' @param REB  Rebounds
#' @param AST  Assists
#' @param STL  Steals
#' @param BLK  Blocks
#' @param FGA  Field goals attempted
#' @param FGM  Field goals made
#' @param FTA  Free throws attempted
#' @param FTM  Free throws made
#' @param TOV  Turnovers
#'
#' @export
#' @examples
#' efficiency(15, 9, 3, 1, 0, 14, 6, 4, 3, 1)
#'

efficiency = function(PTS, REB, AST, STL, BLK, FGA, FGM, FTA, FTM, TOV){
  # https://en.wikipedia.org/wiki/Efficiency_(basketball)
  PTS + REB + AST + STL + BLK - (FGA - FGM) - (FTA - FTM) - TOV
}
