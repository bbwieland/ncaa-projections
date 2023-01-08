ratings_raw = read_csv("https://raw.githubusercontent.com/bbwieland/ncaa-projections/main/data/TeamRatings.csv")
ratings_clean = ratings_raw %>% 
  select(net_rk, team, season_nrtg, season_ortg, off_rk, season_drtg, def_rk, avg_poss, tempo_rk) %>%
  arrange(net_rk) %>%
  mutate(across(c(season_nrtg, season_ortg, season_drtg),function(x) x*100))

schedule = hoopR::kp_fanmatch(Sys.Date())



gameProjector = function(team1, team2, location = "home") {
  
  ncaaAveragePPP = 1.01631
  
  hca_per_poss = 0.04373178
  
  HCA = case_when(
    location == "home" ~ hca_per_poss,
    location == "away" ~ -hca_per_poss,
    TRUE ~ 0
  )
  
  team1Ratings = ratings_raw %>%
    filter(team == team1)
  
  team2Ratings = ratings_raw %>%
    filter(team == team2)
  
  xTeam1PPP = team1Ratings$season_ortg - team2Ratings$season_drtg + ncaaAveragePPP + HCA/2
  xTeam2PPP = team2Ratings$season_ortg - team1Ratings$season_drtg + ncaaAveragePPP - HCA/2
  #xPoss = mean(c(team1Ratings$avg_poss,team2Ratings$avg_poss))
  xPoss = sqrt(team1Ratings$avg_poss*team2Ratings$avg_poss)
  xTeam1PTS = xTeam1PPP*xPoss
  xTeam2PTS = xTeam2PPP*xPoss
  
  data.frame(team_1 = team1, team_1_score = xTeam1PTS, team_2 = team2, team_2_score = xTeam2PTS, poss = xPoss, location = location)
}

