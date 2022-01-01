# library(tidyverse)
# library(ggplot2)
# library(plotly)
# 
# set.seed(339)

# data <- read.csv("data/pokemon.csv")
# types <- read.csv("data/type_chart.csv")

# This let's us find the values for Pokemon in the second quartile
# plot_ly(y = data$Total, type = "box")

# Get the set of Pokemon in the second quartile minus variations on Pokemon
#  Add a flying type 1 Pokemon as there was none in q2
# Get six random Pokemon for each team

# fs <- data %>% 
#   filter(Total > 330 & Total < 450) %>%
#   slice( -(183:185)) %>%
#   slice( -(118:119)) 
# fs$Name[fs$Name == "PumpkabooAverage Size"] <- "Pumpkaboo"
# fs$Name[fs$Name == "WormadamPlantCloak"] <- "Wormadam"
# fs <- rbind(fs, data %>% slice(792))
# 
# team_user <- fs %>% slice(sample(1:181, size=6))
# team_comp <- fs %>% slice(sample(1:181, size=6))


# Determine the faster of the team's two first Pokemon and start the battle
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

speed_test()



# tmp1 <- fs %>% slice(sample(1:181, size=6))
# tmp2 <- fs %>% slice(sample(1:181, size=6))
# 

tmp2$Type.2[1]
if(tmp2$Type.2[1] == "") {
  cat("Hello")
}
  
fs %>% filter(Name == "WormadamPlantCloak")

fs$Name[fs$Name == "WormadamPlantCloak"] <- "Wormadam"
























