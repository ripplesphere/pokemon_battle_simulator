library(shiny)
library(tidyverse)

data <- read.csv("data/pokemon.csv")
types <- read.csv("data/type_chart.csv")

fs <- data %>%
  filter(Total > 330 & Total < 450) %>%
  slice( -(183:185)) %>%
  slice( -(118:119))
fs <- rbind(fs, data %>% slice(792))

type_1 <- unique(fs$Type.1)

team_comp <- fs %>% slice(sample(1:181, size=6))

ui <- fluidPage(
  fluidRow(
    column(4,  
        fluidRow(column(6, 

           selectInput("poke1_type", label = "", 
                       choices = c("Type", type_1),
                       selected = 1)
          ), column(6,
           selectInput("pokemon1", label = "", 
                       choices = c("Select", sort(unique(fs$Name))),
                       selected = 1)
        )),
        fluidRow(column(12, verbatimTextOutput("poke1_val")))
           
    ),
    column(4,
           selectInput("poke2_type", label = "", 
                       choices = c("Type", type_1),
                       selected = 1),
           selectInput("pokemon2", label = h3("Select Pokemon"), 
                       choices = c("Select", sort(unique(fs$Name))),
                       selected = 1),
    ),
    column(4,
           selectInput("poke3_type", label = "", 
                       choices = c("Type", type_1),
                       selected = 1),
           selectInput("pokemon3", label = h3("Select Pokemon"), 
                       choices = c("Select", sort(unique(fs$Name))),
                       selected = 1),
    )
  )
  
)

server <- function(input, output) {
  
  output$poke1_val <- renderPrint({ input$pokemon1 })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
