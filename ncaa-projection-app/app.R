library(shiny)

library(tidyverse)
library(hoopR)
library(glue)

library(reactable)
library(reactablefmtr)

library(shinythemes)

schedule = read_csv("https://raw.githubusercontent.com/bbwieland/ncaa-projections/main/data/Schedule.csv") %>%
  filter(!is.na(prediction) & !is.na(home_team) & !is.na(road_team))

ratings_raw = read_csv("https://raw.githubusercontent.com/bbwieland/ncaa-projections/main/data/TeamRatings.csv")
ratings_clean = ratings_raw %>% 
  select(net_rk, team, season_nrtg, season_ortg, off_rk, season_drtg, def_rk, avg_poss, tempo_rk) %>%
  arrange(net_rk) %>%
  mutate(across(c(season_nrtg, season_ortg, season_drtg),function(x) x*100))


color_pal <- scales::viridis_pal(alpha = 0.5)

color_options = color_pal(101)

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
  #xPoss = sqrt(team1Ratings$avg_poss*team2Ratings$avg_poss)
  
  avg_tempo = mean(ratings_raw$avg_poss)
  
  team_1_diff = team1Ratings$avg_poss - avg_tempo
  team_2_diff = team2Ratings$avg_poss - avg_tempo
  
  xPoss = avg_tempo + team_1_diff + team_2_diff
  
  xTeam1PTS = xTeam1PPP*xPoss
  xTeam2PTS = xTeam2PPP*xPoss
  
  data.frame(team_1 = team1, team_1_score = xTeam1PTS, team_2 = team2, team_2_score = xTeam2PTS, poss = xPoss, location = location)
}

projections_raw = map2_dfr(.x = schedule$home_team, .y = schedule$road_team, .f = ~gameProjector(.x,.y))

projections_processed = projections_raw %>%
  left_join(ratings_raw, by = c("team_1" = "team")) %>%
  left_join(ratings_raw, by = c("team_2" = "team"), suffix = c("_team_1","_team_2")) %>%
  mutate(game_name = glue("{net_rk_team_2} {team_2} at {net_rk_team_1} {team_1}"),
         win_or_loss = ifelse(team_1_score > team_2_score, "W", "L"),
         round_team_1 = round(team_1_score),
         round_team_2 = round(team_2_score)) %>%
  mutate(round_team_1 = ifelse(round_team_1 == round_team_2 & win_or_loss == "W",
                               round_team_1 + 1,
                               round_team_1),
         round_team_2 = ifelse(round_team_1 == round_team_2 & win_or_loss == "L",
                               round_team_2 + 1,
                               round_team_2)) %>%
  mutate(game_proj_result = glue("{ifelse(win_or_loss == 'W',team_1,team_2)},
                                 {ifelse(win_or_loss == 'W',round_team_1,round_team_2)}-{ifelse(win_or_loss == 'W',round_team_2,round_team_1)}")) %>%
  mutate(quality_rank = net_rk_team_1 + net_rk_team_2 - 3,
         quality_index = (1 - quality_rank/(2*nrow(ratings_raw)))*100,
         closeness_index = 100 - 4*abs(team_1_score - team_2_score),
         closeness_index = ifelse(closeness_index < 0, 0, closeness_index))

projections_clean = projections_processed %>%
  select(game_name, game_proj_result, quality_index, closeness_index) %>%
  arrange(-quality_index)
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("College Basketball Team Rankings"),
  tabsetPanel(
    tabPanel(
      "Overall Rankings",
      reactableOutput("rankings")
    ),
    tabPanel(
      "Daily Projections",
      reactableOutput("daily_projections")
    ),
    tabPanel("Methodology",
             h3("Computing Team Rankings"),
             p("The rankings begin with a dataset containing information on every team-game combo over the course of the season. The variables include team score (duh), opponent score (duh), and the game location: home, away, or neutral-site. A fourth variable, possessions, is calculated using the formula Possessions = (FGA + .044*FTA - OREB + TOV)/2 where FGA, FTA, etc. represent the total number of field goals attempted, free throws attempted, etc. for both teams. This is a naive method of estimating possessions, but is useful for its applicability to simple box score statistics."),
             p("From there, each team’s points per possession scored and allowed in each game is calculated with an adjustment applied based on game location. This adjustment is dynamically computed based on the season to date; as of today, January 10, playing at home is worth approximately 4.3 points per 100 possessions in the 2022-23 season."),
             p("Adjusting for game location is particularly important in college basketball, where the level of control teams hold over their home/away splits in non-conference play is almost unparalleled in major sports. For example, Duke played 11 non-conference games this season, none of which were true road games. The Blue Devils have played just two regular season road non-con games over the past four seasons, both as part of the Big Ten-ACC Challenge. Their non-con results are slightly less impressive than a team that put up the same resume while playing road games."),
             p("The same methodology is then carried out for each of the team’s opponents, with games between the team and opponent excluded. Including head-to-head matchups would effectively penalize good teams and reward bad ones, as blowout wins would damage your strength of schedule while blowout losses would bolster it. Their season-long points per possession scored and allowed are calculated with the same home-court adjustment outlined above."),
             p("From there, the team being ranked has its performance against each of its opponents evaluated by calculating the difference between their adjusted PPP scored and allowed and their opponent’s season-long adjusted PPP scored and allowed. This rewards teams more for good performances against good teams, while limiting their benefits from curb-stomping bad teams. Scoring 1.2 PPP against a defense that allows 0.8 PPP is twice as beneficial as putting up that same number against a team that allows 1.0 PPP."),
             p("Finally, the mean of a team’s single-game performances against expected becomes its season-long rating. A team that consistently scores 0.1 PPP more than their opponent’s season average in points per possession allowed would earn an offensive rating of +10 (ratings displayed on the site are per 100 possessions), while a team that holds teams 0.1 PPP below their season-long scoring average would earn a defensive rating of +10. In both cases, higher numbers are better. A team’s net rating is computed as a sum of their offensive and defensive ratings."),
             h3("Computing Daily Projections"),
             p("Projecting a single game from season-long ratings is essentially a three-step process: estimating difference in team strength, adjusting for home-court advantage, and adjusting for tempo."),
             p("First, the difference between two teams is calculated by finding the difference of their season-long offensive and defensive ratings, which ends up being equal to their difference in net rating. These ratings are calculated such that they can be linearly compared: a team with a +20 net rating would be expected to beat a team with a +10 net rating by 10 points if they played 100 possessions."),
             p("Then, a home-court adjustment is applied – the same one incorporated into the overall rankings — to each team’s expected PPP scored and allowed, which are extracted from combining the NCAA’s average points per possession over the course of the season with each team’s offensive and defensive ratings."),
             p("Finally, an estimate for the number of possessions to be played in the game is calculated using a methodology inspired by Ken Pomeroy’s algorithm. Essentially, the total expected possessions is equal to the sum of the differences between each team’s average tempo and the NCAA’s average tempo; if one team plays 4 possessions fewer than average and the other team plays 6 possessions fewer than average, we expect them to combine to play 10 possessions fewer than average."),
             p("Final score projections are calculated by multiplying each team’s expected points per possession by the expected number of possessions played."),
             h3("Comparative Utility"),
             p("People familiar with the world of predictive college basketball analytics will notice methodological similarities to two popular projection systems: KenPom ratings (www.kenpom.com) and T-Rank ratings (www.barttorvik.com). Much of my ratings algorithm is inspired by their methodology, and the bare-bones structure of computing possession-free offensive and defensive ratings that are then adjusted according to strength of schedule is identical."),
             p("This “predictive style” of estimation results in some similar across-the-board results when compared to retrospective team rankings such as the AP Poll. For example, Saint Mary’s is currently 31st in AP ballot voting as of January 10, but ranked 6th in T-Rank, 9th in KenPom, and 10th in my system."),
             p("However, the particular methodologies vary between predictive modeling sites. At the moment, the largest difference between my methodology and their models is the incorporation of a “conference prior” of some sort or adjustment based on conference. This results most clearly in differing evaluations of abnormally strong teams in historically weak conferences or abnormally weak teams in historically strong conferences."),
             p("Two good examples from this season are 14-1 Florida Atlantic, who plays in Conference USA, and 2-14 Louisville, who plays in the ACC. Florida Atlantic ranks 14th in my conference-blind algorithm, 29th in T-Rank, and 38th in KenPom. Louisville ranks 341st in my algorithm,  318th in T-Rank, and 278th in KenPom."),
             p("Whose method is better? Probably theirs. I’m inclined to believe that conference membership provides some valuable information about team strength. In fact, there are all sorts of bits of added complexity incorporated into those two tools which would probably improve my model — for example, instead of a blanket “neutral site” classification (which I use for non-home-or-away games), KenPom uses “semi-home” and “semi-away” classifications for some games."),
             p("All of this is a long-winded way to say that this is an aspirational project: the current methodology for my rankings is pretty solid (I’d be more confident in using it than retrospective rankings for future projection, for example), but not as sophisticated and probably not as good as the cream of the crop. The long-term goal here is to get from the basic algorithm I’ve implemented now to a more effective, more accurate projection system.")
             ),
    tabPanel("Dictionary",
             h3("Overall Ratings Page"),
             p(strong("Net Rating:"),"How many points this team would be expected to outscore the “average” Division I team by in a hypothetical 100-possession game. Measured on the same scale as Ken Pomeroy’s Adjusted Efficiency Margin. Zero is average. Calculated as the sum of each team’s offensive rating and defensive rating."),
             p(strong("Offensive Rating:"),"How many points better per 100 possessions this team’s offense is compared to the average Division I team. Zero is average. A team with an offensive rating of +10 would be expected to score 10 more points than the average Division I team over the course of 100 possessions."),
             p(strong("Defensive Rating:"),"How many points better per 100 possessions this team’s defense is compared to the average Division I team. Zero is average. A team with a defensive rating of +10 would be expected to allow 10 fewer points than the average Division I team over the course of 100 possessions."),
             p(strong("Tempo:"),"The average number of possessions played in this team’s games."),
             h3("Daily Projections Page"),
             p(strong("Quality:"),"A rough estimate of the quality of play in a given game, calculated using the net ratings of the two involved teams. Higher quality = a higher expected level of play. Defined on a scale between 0 and 100 such that a game between the two best teams in the country will have a quality score of 100."),
             p(strong("Closeness:"),"A rough index of how close a game is expected to be. Defined on a scale from 0 to 100 such that a game where two teams are totally equal will have a closeness score of 100, a game expected to be within 10 points will earn a “passing” closeness grade above 60, and games with a 25-point or greater difference in expected margin earn a closeness score of 0.")
  ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  rankings = reactable(ratings_clean,
                       columns = list(
                         team = colDef(style = list(fontWeight = "bold"),
                                       width = 300),
                         season_nrtg = colDef(name = "Net Rating", cell = function(x) sprintf("%+0.2f", x),
                                              sortable=T,
                                              width = 120),
                         season_ortg = colDef(name = "Off. Rating", cell = function(x) sprintf("%+0.2f", x),
                                              sortable=T,
                                              width = 120),
                         season_drtg = colDef(name = "Def. Rating", cell = function(x) sprintf("%+0.2f", x),
                                              sortable=T,
                                              width = 120),
                         avg_poss = colDef(name = "Tempo", cell = function(x) sprintf("%0.1f", x),
                                           sortable=T,
                                           width = 120),
                         net_rk = colDef(name = "Rank",align="right",style = list(fontSize = 18,display = "flex", flexDirection = "column", justifyContent = "center"),
                                         width = 60),
                         off_rk = colDef(name = "",align="center",style = list(fontSize = 18,display = "flex", flexDirection = "column", justifyContent = "center"),width = 60),
                         def_rk = colDef(name = "",align="center",style = list(fontSize = 18,display = "flex", flexDirection = "column", justifyContent = "center"),width = 60),
                         tempo_rk = colDef(name = "",align="center",style = list(fontSize = 18,display = "flex", flexDirection = "column", justifyContent = "center"),width = 60)),
                       
                       theme = fivethirtyeight(font_size = 24),
                       searchable = F,
                       pagination = F
                       )
  output$rankings = renderReactable({rankings})
  
  daily_projections = reactable(projections_clean,
                                columns = list(
                                  game_name = colDef(name = "Game",sortable = F, width = 720),
                                  game_proj_result = colDef(name = "Proj. Result",sortable = F, width = 360),
                                  quality_index = colDef(name = "Quality", width = 120, align = "center", 
                                                         cell = function(x) {
                                                           value = sprintf("%0.1f", x)
                                                           color = color_options[round(x)]
                                                           div(style = list(background = color), value)
                                                           }),
                                  closeness_index = colDef(name = "Closeness", width = 120, align ="center",
                                    cell = function(x) {
                                      value = sprintf("%0.1f", x)
                                      color = color_options[round(x)]
                                      div(style = list(background = color), value)
                                    })
                                ),
                                theme = fivethirtyeight(font_size = 24),
                                searchable = F,
                                pagination = F)
  
  
  output$daily_projections = renderReactable({daily_projections})
}

# Run the application 
shinyApp(ui = ui, server = server)
