library(shiny)
library(tidyverse)

data <- read.csv("data/pokemon.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectizeInput(inputId= "pokeInput", label= "Pokemon", 
                 choices= sort(unique(data$name)), 
                 multiple=T,
                 options = list(plugins= list('remove_button')))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  df_filtered= reactive({
    data %>%
      {if (is.null(data$name)) . else filter(., name %in% data$name)}
  })
  output$tab= renderTable(df_filtered())
}

# Run the application 
shinyApp(ui = ui, server = server)
