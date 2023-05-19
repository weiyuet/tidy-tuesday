########################
# TidyTuesday 2023 w17 #
########################

#### Setup ####
library(tidyverse)
library(janitor)
library(scales)
library(patchwork)

#### Load Data ####
winners <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
london_marathon <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

#### Explore Data ####
range(london_marathon$Date) # Date from 1981 to 2022

london_marathon %>% 
  count(`Official charity`, sort = TRUE) %>% 
  print(n = 36) # At least 35 different official charities

#### Wrangle ####
# Calculate the percent accepted and the percent finished
london_marathon <- london_marathon %>% 
  mutate(percent_accepted = Accepted / Applicants,
         percent_finished = Finishers / Starters)

# Column names to lower case
london_marathon <- london_marathon %>% 
  clean_names()

#### Visualize ####
p1 <- london_marathon %>% 
  select(year,
         percent_accepted) %>% 
  drop_na() %>% 
  ggplot(aes(x = year,
             y = percent_accepted)) +
  geom_line() +
  scale_x_continuous(labels = label_number(big.mark = ""),
                     breaks = seq(min(london_marathon$year), max(london_marathon$year), 5)) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "",
       y = "",
       subtitle = "Percentage of Applicants Accepted in the London Marathon") +
  theme_classic()

p2 <- london_marathon %>% 
  select(year,
         percent_finished) %>% 
  drop_na() %>% 
  ggplot(aes(x = year,
             y = percent_finished)) +
  geom_line() +
  scale_x_continuous(labels = label_number(big.mark = ""),
                     breaks = seq(min(london_marathon$year), max(london_marathon$year), 5)) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "",
       y = "",
       subtitle = "Percentage of Starters that Finished the London Marathon") +
  theme_classic()

# Combine plots
p <- p1/p2

p + plot_annotation(title = "London Marathon: Difficult to get a spot, but most runners finish",
                    caption = "Data: London Marathon R Package\nGraphic: @weiyuet | #TidyTuesday2023 w17")

#### Save Image ####
ggsave("2023/w17/london-marathon.png",
       width = 6,
       height = 6)