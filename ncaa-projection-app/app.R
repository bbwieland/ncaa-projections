library(shiny)

library(tidyverse)
library(reactable)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("College Basketball Team Rankings"),

)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
