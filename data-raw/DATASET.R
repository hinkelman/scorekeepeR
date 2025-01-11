
events = c("FTA", "FTM", "FGA2", "FGM2", "FGA3", "FGM3",
           "STL", "TOL", "DREB", "OREB", "BLK", "AST", "PF")
usethis::use_data(events, overwrite = TRUE)

events_desc = c("Missed free throw", "Made free throw",
                "Missed field goal", "Made field goal",
                "Missed 3-pt field goal", "Made 3-pt field goal",
                "Steal", "Turnover", "Defensive Rebound",
                "Offensive Rebound", "Block", "Assist", "Foul")
usethis::use_data(events_desc, overwrite = TRUE)

