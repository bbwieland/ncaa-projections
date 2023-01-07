library(tidyverse)
library(hoopR)
library(toRvik)

# Get 2022-23 data ----

startDate = as.Date("2022-11-01")
endDate = Sys.Date()

dateRange = seq(from = startDate,to = endDate,by="days")

datesToIterate = gsub("\\-","",as.character(dateRange))

seasonResults = map_dfr(.x = datesToIterate, .f = ~ hoopR::espn_mbb_scoreboard(.x))

# Create modeling data ----

gameData = seasonResults %>%
  filter(status_name == "STATUS_FINAL") %>%
  select(date = game_date, 
         home_team = home_team_location,
         home_id = home_team_id,
         away_team = away_team_location,
         away_id = away_team_id,
         home_score,
         away_score,
         home_win,
         game_id)

teamData = hoopR::espn_mbb_teams()

# Create Elo model ----

library(EloRating)

gameDataEloStructure = gameData %>%
  mutate(winner = ifelse(home_win == 1, home_team, away_team),
         loser = ifelse(home_win == 0, home_team, away_team)) %>%
  select(date,winner,loser) %>%
  arrange(date)

EloModel = elo.seq(winner = gameDataEloStructure$winner,
                   loser = gameDataEloStructure$loser,
                   Date = gameDataEloStructure$date)

OptimalK = optimizek(EloModel)$best$k

EloModel = elo.seq(winner = gameDataEloStructure$winner,
                   loser = gameDataEloStructure$loser,
                   Date = gameDataEloStructure$date,
                   k = OptimalK)

EloResults = extract_elo(EloModel)

# "Expected vs. Actual" Model ----

# methodology:
# 1: for each team, look at their opponent's record/avg pt diff in games NOT vs team
# 2: compare that to team's record/avg pt diff
# 3: residual = "team strength"

# include only DI
teamsToInclude = teamData$team

generateXARanks = function(team,d1_only = T) {
  if (d1_only == T) {
    gameData = gameData %>%
      filter(home_team %in% teamsToInclude & away_team %in% teamsToInclude)
  }
  
  teamGames = gameData %>%
    filter(team == home_team | team == away_team) %>%
    mutate(team_win = ifelse(
      ((team == home_team & home_win == 1) | (team == away_team & home_win == 0)),
      1,
      0
    )) %>%
    mutate(team_pt_diff = ifelse(team == home_team,
                                 home_score - away_score,
                                 away_score - home_score))
  
  teamsInGames = c(teamGames$home_team, teamGames$away_team)
  opponents = teamsInGames[which(teamsInGames != team)]
  
  opponentGames = gameData %>%
    filter(away_team %in% opponents | home_team %in% opponents) %>%
    filter(home_team != team & away_team != team) %>%
    filter(!(home_team %in% opponents & away_team %in% opponents)) %>%
    mutate(opponent_win = ifelse(
      ((away_team %in% opponents & home_win == 0) | (home_team %in% opponents & home_win == 1)),
      1,
      0
    )) %>%
    mutate(opponent_pt_diff = ifelse(home_team %in% opponents,
                                     home_score - away_score,
                                     away_score - home_score))
  
  teamWinPct = mean(teamGames$team_win)
  opponentWinPct = mean(opponentGames$opponent_win)
  teamPerformanceWin = teamWinPct - opponentWinPct
  
  teamPtDiff = mean(teamGames$team_pt_diff)
  opponentPtDiff = mean(opponentGames$opponent_pt_diff)
  teamPerformancePtDiff = teamPtDiff - opponentPtDiff
  
  results = data.frame(Team = team,
                       WinPct = teamPerformanceWin,
                       PtDiff = teamPerformancePtDiff)

  return(results)
}


XAModelResults = map_dfr(.x = teamsToInclude, .f = ~generateXARanks(team = .x))

XAModelResultsRanks = XAModelResults %>%
  mutate(win_pct_rank = nrow(XAModelResults) - dense_rank(WinPct) + 1,
         pt_diff_rank = nrow(XAModelResults) - dense_rank(PtDiff) + 1) %>%
  mutate(composite_score = (win_pct_rank + 3*pt_diff_rank)/4) %>%
  mutate(overall_rank = dense_rank(composite_score))
