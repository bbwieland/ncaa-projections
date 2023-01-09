library(shiny)

library(tidyverse)
library(hoopR)
library(glue)

library(reactable)
library(reactablefmtr)

library(shinythemes)

schedule = read_csv("https://raw.githubusercontent.com/bbwieland/ncaa-projections/main/data/Schedule.csv")
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
  xPoss = sqrt(team1Ratings$avg_poss*team2Ratings$avg_poss)
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
  mutate(quality_rank = net_rk_team_1 + net_rk_team_2,
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
    )
  )

    # Application title
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
