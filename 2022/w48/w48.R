###################
# tidytuesday w48 #
###################

#### Setup ####
library(tidyverse)
library(scales)

#### Load data ####
wcmatches <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

###################################
# How many goals scored per game? #
###################################

#### Calculate goals per game ####
worldcups <- worldcups %>% 
  mutate(goals_per_game = goals_scored/games)

#### Plot goals per game over time ####
worldcups %>% 
  ggplot(aes(x = year, y = goals_per_game)) +
  geom_step() +
  geom_hline(yintercept = 3, linetype = "dashed", colour = "red") +
  scale_x_continuous(breaks = seq(1930, 2018, 8)) +
  scale_y_continuous(limits = c(2, 6),
                     breaks = seq(1, 6, 1)) +
  theme_classic() +
  labs(x = "", y = "Goals per game",
       title = "Number of goals per game in the FIFA World Cup",
       subtitle = "Goals scored per game is less than 3 in the modern era",
       caption = "Data: Kaggle - FIFA World Cup\nGraphic: @weiyuet | #TidyTuesday2022 w48")

#### Save image ####
ggsave("2022/w48/goals-per-game.png", width = 7, height = 5)