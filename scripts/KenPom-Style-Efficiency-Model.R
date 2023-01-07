library(tidyverse)
library(hoopR)

# Import data from scraper ----

gameData = read_csv("KenPomGames.csv")

extractWinningScore = function(x) {
  gsub("\\w,\\s","",x) %>%
    gsub("-.*","",.) %>%
    as.numeric()
}

extractLosingScore = function(x) {
  gsub("\\w,\\s","",x) %>%
    gsub(".*-","",.) %>%
    as.numeric()
}

extractWinLoss = function(x) {
  ifelse(substr(x,1,1) == "W",1,0)
}

validGames = gameData %>%
  filter(!is.na(team_rk)) %>%
  filter(!is.na(opponent_rk)) %>%
  mutate(win_loss = extractWinLoss(result),
         team_score = ifelse(win_loss == 1, extractWinningScore(result),extractLosingScore(result)),
         opp_score = ifelse(win_loss == 0, extractWinningScore(result),extractLosingScore(result))) %>%
  mutate(team_ppp = team_score/poss,
         opp_ppp = opp_score/poss)

validTeams = unique(validGames$team)

## Team rating formula ----

generateTeamRating = function(inputTeam) {
  
  teamGames = validGames %>%
    filter(team == inputTeam)
  
  opponents = unique(teamGames$opponent)
  
  opponentGames = validGames %>%
    filter(team %in% opponents & opponent != inputTeam)
  
  opponentRatings = opponentGames %>%
    group_by(team) %>%
    filter(opponent != inputTeam) %>%
    summarise(season_ppp_opp_off = sum(team_score)/sum(poss),
              season_ppp_opp_def = sum(opp_score)/sum(poss))
  
  teamGamesWithOppRtg = teamGames %>%
    inner_join(opponentRatings, by = c("opponent" = "team"))
  
  teamRatings = teamGamesWithOppRtg %>%
    mutate(game_off_rtg_vs_avg = team_ppp - season_ppp_opp_def,
           game_def_rtg_vs_avg = season_ppp_opp_off - opp_ppp)
  
  teamSummaryRatings = teamRatings %>%
    group_by(team) %>%
    summarise(season_ortg = mean(game_off_rtg_vs_avg),
              season_drtg = mean(game_def_rtg_vs_avg),
              season_nrtg = season_ortg + season_drtg,
              avg_poss = mean(poss))
  
  teamSummaryRatings

}

teamRatings = map_dfr(.x = validTeams, .f = ~ generateTeamRating(.x))

teamRatingsFinal = teamRatings %>%
  mutate(off_rk = dense_rank(desc(season_ortg)),
         def_rk = dense_rank(desc(season_drtg)),
         net_rk = dense_rank(desc(season_nrtg)))

# Game predictions ----

## need to set a baseline variable for xPPP

gameProjector = function(team1, team2, location = "neutral") {
  
  ncaaAveragePPP = (sum(validGames$team_score) + sum(validGames$opp_score))/(2*sum(validGames$poss))
  
  HCA_Value = 4.5
  
  HCA = case_when(
    location == "home" ~ HCA_Value,
    location == "away" ~ -HCA_Value,
    location == "neutral" ~ 0
  )
  
  team1Ratings = teamRatings %>%
    filter(team == team1)
  
  team2Ratings = teamRatings %>%
    filter(team == team2)
  
  xTeam1PPP = team1Ratings$season_ortg - team2Ratings$season_drtg + HCA/200 + ncaaAveragePPP
  xTeam2PPP = team2Ratings$season_ortg - team1Ratings$season_drtg - HCA/200 + ncaaAveragePPP
  #xPoss = mean(c(team1Ratings$avg_poss,team2Ratings$avg_poss))
  xPoss = sqrt(team1Ratings$avg_poss*team2Ratings$avg_poss)
  xTeam1PTS = xTeam1PPP*xPoss
  xTeam2PTS = xTeam2PPP*xPoss
  
  data.frame(team_1 = team1, team_1_score = xTeam1PTS, team_2 = team2, team_2_score = xTeam2PTS, poss = xPoss, location = location)
}

gameProjector("Virginia Tech","Virginia","away")
