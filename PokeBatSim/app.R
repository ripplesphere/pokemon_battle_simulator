library(shiny)
library(tidyverse)

data <- read.csv("data/pokemon.csv")
types <- read.csv("data/type_chart.csv")

fs <- data %>%
  filter(Total > 330 & Total < 450) %>%
  slice( -(183:185)) %>%
  slice( -(118:119))
fs$Name[fs$Name == "PumpkabooAverage Size"] <- "Pumpkaboo"
fs$Name[fs$Name == "WormadamPlantCloak"] <- "Wormadam"
fs <- rbind(fs, data %>% slice(792))

type_1 <- unique(fs$Type.1)

team_user <- c()
team_comp <- fs %>% slice(sample(1:181, size=6))

ui <- navbarPage(
  title="Pokemon Battle Simulator",
  id="pages",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  shinyjs::useShinyjs(),
  tabPanel(title="Select Team",
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
        fluidRow(column(12, htmlOutput("poke1_val") %>% 
                          tagAppendAttributes(class = 'pokemon_stats')))
           
    ),
    column(4,
           fluidRow(column(6, 
                       selectInput("poke2_type", label = "", 
                       choices = c("Type", type_1),
                       selected = 1)
           ), column(6,
                     selectInput("pokemon2", label = "", 
                       choices = c("Select", sort(unique(fs$Name))),
                       selected = 1)
           )),
           fluidRow(column(12, htmlOutput("poke2_val") %>% 
                             tagAppendAttributes(class = 'pokemon_stats')))
    ),
    column(4,
           fluidRow(column(6, 
                       selectInput("poke3_type", label = "", 
                       choices = c("Type", type_1),
                       selected = 1)
           ), column(6,
                    selectInput("pokemon3", label = "", 
                       choices = c("Select", sort(unique(fs$Name))),
                       selected = 1)
           )),
           fluidRow(column(12, htmlOutput("poke3_val") %>% 
                             tagAppendAttributes(class = 'pokemon_stats')))
    )
  ),
  fluidRow(
    column(4,  
           fluidRow(column(6, 
                           selectInput("poke4_type", label = "", 
                                       choices = c("Type", type_1),
                                       selected = 1)
           ), column(6,
                     selectInput("pokemon4", label = "", 
                                 choices = c("Select", sort(unique(fs$Name))),
                                 selected = 1)
           )),
           fluidRow(column(12, htmlOutput("poke4_val") %>% 
                             tagAppendAttributes(class = 'pokemon_stats')))
           
    ),
    column(4,
           fluidRow(column(6, 
                           selectInput("poke5_type", label = "", 
                                       choices = c("Type", type_1),
                                       selected = 1)
           ), column(6,
                     selectInput("pokemon5", label = "", 
                                 choices = c("Select", sort(unique(fs$Name))),
                                 selected = 1)
           )),
           fluidRow(column(12, htmlOutput("poke5_val") %>% 
                             tagAppendAttributes(class = 'pokemon_stats')))
    ),
    column(4,
           fluidRow(column(6, 
                           selectInput("poke6_type", label = "", 
                                       choices = c("Type", type_1),
                                       selected = 1)
           ), column(6,
                     selectInput("pokemon6", label = "", 
                                 choices = c("Select", sort(unique(fs$Name))),
                                 selected = 1)
           )),
           fluidRow(column(12, htmlOutput("poke6_val") %>% 
                             tagAppendAttributes(class = 'pokemon_stats')))
    )
  ),
  fluidRow(
    column(2, offset = 10,  
           actionButton("to_battle", label = "Battle")
           
    )),
    fluidRow(column(6, htmlOutput("value")))
  ),
  tabPanel(title="Battle", id="battle_panel", 
           fluidRow(column(3, offset = 2, 
                    htmlOutput("user_team") %>% 
                      tagAppendAttributes(class = 'pokemon_stats')),
                    column(2,
                           actionButton("start_battle", label = "Begin") %>% 
                             tagAppendAttributes(class = 'begin_btn')),
                    column(4,
                    htmlOutput("comp_team") %>% 
                      tagAppendAttributes(class = 'pokemon_stats'))
           ),
           fluidRow(column(6, offset = 2, htmlOutput("battle_results")))
))

################################################################################
server <- function(input, output, session) {

  pokemon_list <- function(poke_type) {
      if (poke_type == "Type") {
        pokemon <- unique(sort(fs$Name))
      } else {
        pokemon <- fs %>%
          filter(`Type.1` == poke_type) %>%
          pull(Name) %>% unique() %>% sort()
      }
      return(pokemon)
  }
  
  observe({
    updateSelectInput(session, "pokemon1",
                      choices = c("Select", pokemon_list(input$poke1_type))
    )
  })
  
  observe({
    updateSelectInput(session, "pokemon2",
                      choices = c("Select", pokemon_list(input$poke2_type))
    )
  })
  
  observe({
    updateSelectInput(session, "pokemon3",
                      choices = c("Select", pokemon_list(input$poke3_type))
    )
  })
  
  observe({
    updateSelectInput(session, "pokemon4",
                      choices = c("Select", pokemon_list(input$poke4_type))
    )
  })
  
  observe({
    updateSelectInput(session, "pokemon5",
                      choices = c("Select", pokemon_list(input$poke5_type))
    )
  })
  
  observe({
    updateSelectInput(session, "pokemon6",
                      choices = c("Select", pokemon_list(input$poke6_type))
    )
  })
  
  pokemon_output <- function(pokemon_name) {
    pokemon <- fs %>% filter(Name == pokemon_name)
      output_str <- paste0("Name: <b>",pokemon$Name,"</b><br>",
                           "Type:&nbsp&nbsp&nbsp<b>",pokemon$Type.1)
      if(pokemon$Type.2 != "") {
        output_str <- paste0(output_str,", ",pokemon$Type.2)
      }
      output_str <- paste0(output_str,"</b><br>",
                           "Total:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp<b>",
                           pokemon$Total,"</b><br>",
                           "Attack:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp<b>",
                           pokemon$Attack,"</b><br>",
                           "Defense:&nbsp&nbsp&nbsp<b>",
                           pokemon$Defense,"</b><br>",
                           "Sp. Atk:&nbsp&nbsp&nbsp&nbsp&nbsp<b>",
                           pokemon$Sp..Atk,"</b><br>",
                           "Sp. Def:&nbsp&nbsp&nbsp&nbsp<b>",
                           pokemon$Sp..Def,"</b><br>",
                           "Speed:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp<b>",
                           pokemon$Speed,"</b><br>",
                           "Generation:&nbsp<b>",pokemon$Generation,"</b><br>")
      if(pokemon$Legendary == "True") {
        output_str <- paste0(output_str,"<b>Legendary</b>")
      }
      return(output_str)
  }
  
  output$poke1_val <- renderUI({
    if (input$pokemon1 == "Select") {
      output_str <- "Select a Pokemon<br><br><br><br><br><br><br><br><br>"
    } else {
      output_str <- pokemon_output(input$pokemon1)
    }
    HTML(output_str)
  })
  
  output$poke2_val <- renderUI({
    if (input$pokemon2 == "Select") {
      output_str <- "Select a Pokemon"
    } else {
      output_str <- pokemon_output(input$pokemon2)
    }
    HTML(output_str)
  })
  
  output$poke3_val <- renderUI({
    if (input$pokemon3 == "Select") {
      output_str <- "Select a Pokemon"
    } else {
      output_str <- pokemon_output(input$pokemon3)
    }
    HTML(output_str)
  })
  
  output$poke4_val <- renderUI({
    if (input$pokemon4 == "Select") {
      output_str <- "Select a Pokemon<br><br><br><br><br><br><br><br><br>"
    } else {
      output_str <- pokemon_output(input$pokemon4)
    }
    HTML(output_str)
  })
  
  output$poke5_val <- renderUI({
    if (input$pokemon5 == "Select") {
      output_str <- "Select a Pokemon"
    } else {
      output_str <- pokemon_output(input$pokemon5)
    }
    HTML(output_str)
  })
  
  output$poke6_val <- renderUI({
    if (input$pokemon6 == "Select") {
      output_str <- "Select a Pokemon"
    } else {
      output_str <- pokemon_output(input$pokemon6)
    }
    HTML(output_str)
  })
  
  observe({
    
    if (input$pokemon1 == 'Select' || input$pokemon2 == 'Select' ||
        input$pokemon3 == 'Select' || input$pokemon4 == 'Select' ||
        input$pokemon5 == 'Select' || input$pokemon6 == 'Select') {
      shinyjs::disable("to_battle")
      shinyjs::disable(selector = 'a[data-value="Battle"')
    } else {
      shinyjs::enable("to_battle")
      shinyjs::enable(selector = 'a[data-value="Battle"')
      team_user <- fs %>% filter(Name == input$pokemon1)
      team_user <- rbind(team_user, fs %>% filter(Name == input$pokemon2))
      team_user <- rbind(team_user, fs %>% filter(Name == input$pokemon3))
      team_user <- rbind(team_user, fs %>% filter(Name == input$pokemon4))
      team_user <- rbind(team_user, fs %>% filter(Name == input$pokemon5))
      team_user <- rbind(team_user, fs %>% filter(Name == input$pokemon6))
      output$user_team <- renderUI({
        HTML(paste0("<h3>User's Team</h3>",input$pokemon1,"<br>",
               input$pokemon2,"<br>",input$pokemon3,"<br>",
               input$pokemon4,"<br>",input$pokemon5,"<br>",input$pokemon6))
      })
      output$comp_team <- renderUI({
        HTML(paste0("<h3>Computer's Team</h3>",team_comp$Name[1],"<br>",
               team_comp$Name[2],"<br>",team_comp$Name[3],"<br>",
               team_comp$Name[4],"<br>",team_comp$Name[5],"<br>",
               team_comp$Name[6]))
      })
    }
  })
  
  observeEvent(input$to_battle, {
    updateTabsetPanel(session, "pages",
                      selected = "Battle"
    )
  })
  
  ######################################################
  ### Battle Page
  ######################################################
  speed_test <- function() {
    
    if (team_user[1,"Speed"] > team_comp[1,"Speed"]) {
      battle("user", 1, team_user, "comp", 1, team_comp)
    } else {
      battle("comp", 1, team_comp, "user", 1, team_user)
    }
    
  }
  
  battle <- function(atk_name,atk_poke,atk_team,def_name,def_poke,def_team) {
    attack_modifier <- types %>%
      filter(Attacking == atk_team$Type.1[atk_poke]) %>%
      pull(def_team$Type.1[def_poke])
    
    remaining_def <- def_team$Defense[def_poke] -
      atk_team$Attack[atk_poke] * attack_modifier
    
    if (remaining_def <= 0) {
      cat(atk_team$Name[atk_poke],
          " beats ", def_team$Name[def_poke], "\n")
      def_poke <- def_poke + 1
      if (def_poke > 6) {
        return(atk_name)
      }
    } else {
      def_team$Defense[def_poke] <- remaining_def
    }
    
    battle(def_name,def_poke,def_team,atk_name,atk_poke,atk_team)
    
  }
  
  observeEvent(input$start_battle, {
    output$start_battle <- renderUI({
      HTML("hello") #speed_test())
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)













