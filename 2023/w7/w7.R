#######################
# TidyTuesday 2023 w7 #
#######################

#### Setup ####
library(tidyverse)

#### Load data ####
age_gaps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

#### Wrangle ####

#### Visualize ####
directors <- c("Woody Allen", "John Glen", "Martin Scorsese", "Mike Newell", "Steven Spielberg", "David Fincher")

age_gaps %>% 
  filter(director %in% directors) %>% 
  ggplot(aes(x = release_year,
             y = age_difference,
             colour = director)) +
  geom_point(show.legend = TRUE) +
  geom_line() +
  labs(x = "Release Year",
       y = "Age Difference")