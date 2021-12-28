library(tidyverse)
library(ggplot2)
library(plotly)

set.seed(339)

data <- read.csv("data/pokemon.csv")
types <- read.csv("data/type_chart.csv")

# This let's us find the values for Pokemon in the second quartile
plot_ly(y = data$Total, type = "box")

# Get the set of Pokemon in the second quartile minus variations on Pokemon
fs <- data %>% 
  filter(Total > 330 & Total < 450) %>%
  slice( -(183:185)) %>%
  slice( -(118:119)) 
#  Add a flying type 1 Pokemon as there was none in q2
fs <- rbind(fs, data %>% slice(792))

# Get six random Pokemon for each team
team_user <- fs %>% slice(sample(1:181, size=6))
team_comp <- fs %>% slice(sample(1:181, size=6))

# Determine the faster of the team's two first Pokemon and start the battle
ifelse (team_user[1,"Speed"] > team_comp[1,"Speed"],
        start_battle(c("user", "comp"),list(team_user, team_comp)), 
        start_battle(c("comp", "user"),list(team_comp, team_user)))

# This function will determine the winning team
start_battle <- function(team_names,team_teams) {
  winner <- calc_def(1,1)
  
  # This function acts as the battle mechanism for each team
  #  calling itself switching which team it is representing each time
  calc_def <- function(team1_poke,team2_poke) {
    # Determine the attack modifier for the attacking Pokemon against the
    #  defending Pokemon
    attack_modifier <- types %>% 
      filter(Attacking == team_teams %>% pluck(1, 1, "Type.1") %>% 
               `[[`(team1_poke)) %>% 
      pull(team_teams %>% pluck(2, 1, "Type.1") %>% `[[`(team2_poke))
    # Calculate the defense remaining for the Pokemon after the attack
    remaining_def <- team_teams %>% pluck(2, 1, "Defense")  %>% 
      `[[`(team2_poke) -
      team_teams %>% pluck(1, 1, "Attack")  %>% 
      `[[`(team1_poke) * attack_modifier
    # here, or somewhere, will need to determine counter attack, switch
    #  to next Pokemon, or team defeated
    
    team_names <- replace(team_names, c(1,2), team_names[c(2,1)])
    team_teams <- list(team_teams[2], team_teams[1])
    calc_def(team2_poke, team1_poke)
  }
  
}

                     




















