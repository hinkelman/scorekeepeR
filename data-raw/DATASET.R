
events = c("FTM", "FTA", "FGM2", "FGA2", "FGM3", "FGA3",
           "STL", "TOV", "DREB", "OREB", "BLK", "AST", "PF")
usethis::use_data(events, overwrite = TRUE)

events_desc = c("Made free throw", "Missed free throw",
                "Made field goal", "Missed field goal",
                "Made 3-pt field goal", "Missed 3-pt field goal",
                "Steal", "Turnover", "Defensive Rebound",
                "Offensive Rebound", "Block", "Assist", "Foul")
usethis::use_data(events_desc, overwrite = TRUE)

