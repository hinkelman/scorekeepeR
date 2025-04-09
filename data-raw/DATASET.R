
events = c("FTM", "FTA", "FGM2", "FGA2", "FGM3", "FGA3",
           "STL", "TOV", "DREB", "OREB", "BLK", "AST", "PF")
usethis::use_data(events, overwrite = TRUE)

events_desc = c("Made free throw", "Missed free throw",
                "Made field goal", "Missed field goal",
                "Made 3-pt field goal", "Missed 3-pt field goal",
                "Steal", "Turnover", "Defensive Rebound",
                "Offensive Rebound", "Block", "Assist", "Foul")
usethis::use_data(events_desc, overwrite = TRUE)

# counting stats stored in GameStats.csv
# points and rebounds are only columns that could be calculated from other columns
stats_cols = c("FTM", "FTA", "FGM2", "FGA2", "FGM3", "FGA3", "TOV", "STL",
               "DREB", "OREB", "BLK", "AST", "PF", "PTS", "REB", "DNP")

# common columns used for statistics display
stats_display_cols = c("PTS", "FGM", "FGA", "FG%", "3PM" = "FGM3", "3PA" = "FGA3", "3P%",
                       "FTM", "FTA", "FT%", "TS%", "OREB", "DREB", "REB",
                       "AST", "TOV", "STL", "BLK", "PF")
usethis::use_data(stats_display_cols, overwrite = TRUE)

