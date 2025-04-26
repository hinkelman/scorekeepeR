
#' Points
#'
#' Calculate points scored
#'
#' @md
#' @param FTM   Free throws made
#' @param FGM2  Two-point field goals made
#' @param FGM3  Three-point field goals made
#'
#' @export
#'

calc_points <- function(FTM, FGM2, FGM3){
  FTM + (FGM2 * 2) + (FGM3 * 3)
}

#' Shooting percentage
#'
#' Calculate shooting percentage
#'
#' @md
#' @param made        Shots made
#' @param attempted   Shots attempted
#'
#' @export
#'

calc_shooting <- function(made, attempted){
  if (any(made > attempted)) warning("Made is greater than attempted")
  pct = ifelse(attempted == 0, NA, made/attempted * 100)
  if(any(!is.na(pct) & pct > 100)) warning("Shooting percentage exceeds 100%")
  round(pct)
}

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
#' calc_true_shooting(18, 4, 12)
#'

calc_true_shooting <- function(PTS, FTA, FGA){
  # https://en.wikipedia.org/wiki/True_shooting_percentage
  ts = ifelse(FTA == 0 & FGA == 0, NA, PTS/(0.88 * FTA + 2 * FGA) * 100)
  if(any(!is.na(ts) & ts > 150)) warning("TS% exceeds maximum value (150%)")
  round(ts)
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
#' calc_efficiency(15, 9, 3, 1, 0, 14, 6, 4, 3, 1)
#'

calc_efficiency = function(PTS, REB, AST, STL, BLK, FGA, FGM, FTA, FTM, TOV){
  # https://en.wikipedia.org/wiki/Efficiency_(basketball)
  PTS + REB + AST + STL + BLK - (FGA - FGM) - (FTA - FTM) - TOV
}

#' Game statistics
#'
#' Create dataframe with tallied and calculated game statistics
#'
#' @md
#' @param data    Dataframe that includes columns with tallied game statistics
#'
#' @export
#'

calc_game_stats <- function(data){
  data |>
    dplyr::mutate(PTS = calc_points(FTM, FGM2, FGM3),
                  REB = OREB + DREB,
                  `FT%` = calc_shooting(FTM, FTA),
                  FGM = FGM2 + FGM3,
                  FGA = FGA2 + FGA3,
                  `FG%` = calc_shooting(FGM, FGA),
                  `3P%` = calc_shooting(FGM3, FGA3),
                  `TS%` = calc_true_shooting(PTS, FTA, FGA),
                  EFF = calc_efficiency(PTS, REB, AST, STL, BLK, FGA, FGM, FTA, FTM, TOV))
}

