library(tidyverse)
library(hoopR)

# Import data from scraper ----

gameData = read_csv("/Users/ben/Desktop/ncaa-projections/data/KenPomGames.csv")

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

write_csv(validGames, "/Users/ben/Desktop/ncaa-projections/data/KenPomGamesCleaned.csv")

validTeams = unique(validGames$team)

## Team rating formula ----

inputTeam = "Virginia"
generateTeamRating = function(inputTeam) {
  
  ## calculate magnitude of home-court adjustment
  # note: numbers for average HCA and tempo pulled from kenpom
  ncaa_avg_hca = 3.0
  ncaa_avg_tempo = 68.6
  ncaa_hca_per_100 = ncaa_avg_hca * (100/ncaa_avg_tempo)
  ncaa_hca_per_poss = ncaa_hca_per_100 / 100
  
  ## grab team games
  teamGames = validGames %>%
    filter(team == inputTeam)
  
  ## apply home court adjustment to team games
  teamGames = teamGames %>%
    mutate(home_away_adjustment = case_when(
      location == "Home" ~ ncaa_hca_per_poss,
      location == "Away" ~ -ncaa_hca_per_poss,
      TRUE ~ 0
    )) %>%
    mutate(team_ppp = team_ppp - home_away_adjustment/2,
           opp_ppp = opp_ppp + home_away_adjustment/2)
  
  ## grab opponents
  opponents = unique(teamGames$opponent)
  
  ## grab data from all opponent games
  opponentGames = validGames %>%
    filter(team %in% opponents & opponent != inputTeam)
  
  ## compute opponent ratings in games NOT against team
  opponentRatings = opponentGames %>%
    group_by(team) %>%
    filter(opponent != inputTeam) %>%
    # apply home court adjustment
    mutate(home_away_adjustment = case_when(
      location == "Home" ~ ncaa_hca_per_poss*poss,
      location == "Away" ~ -ncaa_hca_per_poss*poss,
      TRUE ~ 0
    )) %>%
    mutate(team_score = team_score - home_away_adjustment/2,
           opp_score = opp_score + home_away_adjustment/2) %>%
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
         net_rk = dense_rank(desc(season_nrtg)),
         tempo_rk = dense_rank(desc(avg_poss)))

write_csv(teamRatingsFinal,"/Users/ben/Desktop/ncaa-projections/data/TeamRatings.csv")
