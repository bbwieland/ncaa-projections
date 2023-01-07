library(shiny)

library(tidyverse)

library(reactable)
library(reactablefmtr)

ratings_raw = read_csv("https://raw.githubusercontent.com/bbwieland/ncaa-projections/main/data/TeamRatings.csv")
ratings_clean = ratings_raw %>% 
  select(net_rk, team, season_nrtg, season_ortg, off_rk, season_drtg, def_rk, avg_poss, tempo_rk) %>%
  arrange(net_rk) %>%
  mutate(across(c(season_nrtg, season_ortg, season_drtg),function(x) x*100))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("College Basketball Team Rankings"),
  reactableOutput("rankings")
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
}

# Run the application 
shinyApp(ui = ui, server = server)
