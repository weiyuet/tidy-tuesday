###################
# tidytuesday w48 #
###################

#### Setup ####
library(tidyverse)
library(scales)

#### Load data ####
wcmatches <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

#############
# How many goals are scored? #
#############

#### Calculate goals per game and goals per team ####
worldcups <- worldcups %>% 
  mutate(goals_per_game = goals_scored/games,
         goals_per_team = goals_scored/teams)

#### Plot goals per game over time ####
worldcups %>% 
  ggplot(aes(x = year, y = goals_per_game)) +
  geom_step() +
  scale_x_continuous(breaks = seq(1930, 2018, 4))

#### Plot goals per game over time ####
worldcups %>% 
  ggplot(aes(x = year, y = goals_per_team)) +
  geom_step() +
  scale_x_continuous(breaks = seq(1930, 2018, 4))