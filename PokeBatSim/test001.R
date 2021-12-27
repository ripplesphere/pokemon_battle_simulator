library(tidyverse)
library(ggplot2)
library(plotly)

# https://rviews.rstudio.com/2020/07/20/shallow-neural-net-from-scratch-using-r-part-1/

data <- read.csv("data/pokemon.csv")
# https://github.com/zonination/pokemon-chart/blob/master/chart.csv
types <- read.csv("data/type_chart.csv")

plot_ly(y = data$Total, type = "box")

first_set <- data %>% 
  filter(Total > 330 & Total < 450) %>%
  slice( -(183:185)) %>%
  slice( -(118:119)) 
fs <- rbind(first_set, data %>% slice(792))

team1 <- fs %>% slice(sample(1:185, size=6))
team2 <- fs %>% slice(sample(1:185, size=6))

team1$Name
team2$Name

sample(0:1,1)

team1_poke <- 1
team2_poke <- 1


while(team1_poke <= 6 && team2_poke <= 6) {
  if(team1[team1_poke,"Total"] > team1[team2_poke,"Total"]) {
    print(paste0("team 1 won: ", team1[team1_poke,"Name"]))
    team1_poke <- team1_poke + 1
  } else {
    print(paste0("team 2 won: ", team1[team2_poke,"Name"]))
    team2_poke <- team2_poke + 1
  }
}
print(paste0("Match winner: ", ifelse (team1_poke > team2_poke, "Team 1", "Team 2")))






team2[team2_poke,"Defense"]
team1[team1_poke,"Attack"]



poke_battle <- function(poke_a, poke_d) {
  attack_modifier <- types %>% 
    filter(Attacking == poke_a[1,3]) %>% pull(poke_d[1,3])
  return(poke_d[1,"Defense"] - poke_a[1,"Attack"] * attack_modifier)
}

team_battle <- function(team_a, team_b) {
  team_a_poke <- 1
  team_b_poke <- 1
  if (poke_battle(team_a[team_a_poke,], team_b[team_b_poke,]) > 0) {
    print("hello")
  }
}

ifelse (team1[team1_poke,"Speed"] > team2[team2_poke,"Speed"],
        poke_battle(team1[team1_poke,], team2[team2_poke,]), 
        poke_battle(team2[team2_poke,], team1[team1_poke,]))


battle_field <- c(1,1,1,team1,team2)


battle_field$team1


team1[team1_poke,]
team2[team2_poke,]
























