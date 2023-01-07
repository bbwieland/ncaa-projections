library(tidyverse)
library(hoopR)
library(glue)

# Get 2022-23 data ----

startDate = as.Date("2022-11-01")
endDate = Sys.Date()

dateRange = seq(from = startDate,to = endDate,by="days")

datesToIterate = gsub("\\-","",as.character(dateRange))

seasonResults = map_dfr(.x = datesToIterate, .f = ~ hoopR::espn_mbb_scoreboard(.x))

validTeams = hoopR::espn_mbb_teams()
validTeamIDs = validTeams$team_id

# Coerce data into desired format for creating team estimates ---- 

validGames = seasonResults %>%
  filter(home_team_id %in% validTeamIDs & away_team_id %in% validTeamIDs)

gamesToIterate = unique(validGames$game_id)

createCleanBoxScoreRow = function(game_id) {
  
  boxScore = try({hoopR::espn_mbb_team_box(game_id)})
  
  if (!is.null(nrow(boxScore)) & !is.null(boxScore$field_goals_made_field_goals_attempted)) {
    
    grabMakes = function(x) {
      gsub("-.*","",x) %>%
        as.numeric()
    }
    
    grabAttempts = function(x) {
      gsub(".*-","",x) %>%
        as.numeric()
    }
    
    cleanHomeAway = function(x) {
      case_when(x == "HOME" ~ "AWAY",
                x == "AWAY" ~ "HOME")
    }
    
    boxScoreShotsAdded = boxScore %>%
      mutate(fgm = grabMakes(field_goals_made_field_goals_attempted),
             fga = grabAttempts(field_goals_made_field_goals_attempted),
             fg3m = grabMakes(three_point_field_goals_made_three_point_field_goals_attempted),
             fg3a = grabAttempts(three_point_field_goals_made_three_point_field_goals_attempted),
             fg2m = fgm - fg3m,
             fg2a = fga - fg3a,
             ftm = grabMakes(free_throws_made_free_throws_attempted),
             fta = grabAttempts(free_throws_made_free_throws_attempted),
             points = 2*fg2m + 3*fg3m + ftm,
             oreb = as.numeric(offensive_rebounds),
             to = as.numeric(turnovers),
             home_away = cleanHomeAway(home_away)) %>%
      mutate(poss_estim = 0.96*(fga + to + 0.44*fta - oreb))
    
    boxScoreCleanRow = boxScoreShotsAdded %>%
      mutate(home_away = tolower(home_away)) %>%
      select(game_id,home_away,team = team_location,
             fgm,fga,fg2m,fg2a,fg3m,fg3a,ftm,fta,points,oreb,to,poss_estim) %>%
      pivot_wider(names_from = home_away, values_from = team:poss_estim, names_glue = "{.value}_{home_away}")
    
    boxScoreCleanRowUpdated = boxScoreCleanRow %>%
      mutate(poss_estim_final = (poss_estim_home + poss_estim_away)/2) %>%
      mutate(points_home_adj = points_home - 1.5,
             points_away_adj = points_away + 1.5) %>%
      mutate(ppp_home = points_home/poss_estim_final,
             ppp_away = points_away/poss_estim_final) %>%
      mutate(ppp_home_adj = points_home_adj/poss_estim_final,
             ppp_away_adj = points_away_adj/poss_estim_final)
    
    boxScoreCleanRowUpdated
    
  }

}

boxScoreResults = map_dfr(.x = gamesToIterate, .f = ~ createCleanBoxScoreRow(game_id = .x))
