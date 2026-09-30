#' Game event columns
#' @export
events = c("FTM", "FTA", "FGM2", "FGA2", "FGM3", "FGA3",
           "STL", "TOV", "DREB", "OREB", "BLK", "AST", "PF")

#' Game event description columns
#' @export
events_desc = c("Made free throw", "Missed free throw",
                "Made field goal", "Missed field goal",
                "Made 3-pt field goal", "Missed 3-pt field goal",
                "Steal", "Turnover", "Defensive Rebound",
                "Offensive Rebound", "Block", "Assist", "Foul")

#' Counting stat columns
#' @export
stats_cols = c("FTM", "FTA", "FGM2", "FGA2", "FGM3", "FGA3", "TOV", "STL",
               "DREB", "OREB", "BLK", "AST", "PF", "PTS", "REB", "DNP")

#' Statistical display columns
#' @export
stats_display_cols = c("PTS", "FGM", "FGA", "FG%", "3PM" = "FGM3", "3PA" = "FGA3", "3P%",
                       "FTM", "FTA", "FT%", "TS%", "OREB", "DREB", "REB",
                       "AST", "TOV", "STL", "BLK", "PF")

