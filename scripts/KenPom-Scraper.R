library(tidyverse)
library(hoopR)

# Get data ----

hoopR::login(user_email = "benbowenwieland@gmail.com",
             user_pw = "Embiid21$$")

schedule = hoopR::kp_fanmatch(Sys.Date()) %>%
  filter(!is.na(home_team) & !is.na(road_team))

write_csv(schedule,"/Users/ben/Desktop/ncaa-projections/data/Schedule.csv")

# get teams from current season

currentSeason = 2023
currentKPRatings = hoopR::kp_pomeroy_ratings(currentSeason,currentSeason)
currentTeams = unique(currentKPRatings$team)

# # iterate through all games from current season

gameData = map_dfr(.x = currentTeams, .f = ~ hoopR::kp_team_schedule(.x,currentSeason))

write_csv(gameData,"/Users/ben/Desktop/ncaa-projections/data/KenPomGames.csv")