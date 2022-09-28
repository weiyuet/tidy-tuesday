# Setup
library(tidyverse)
library(skimr)

# Load data
ultra_rankings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# Wrangle data
race <- race %>% 
  mutate(participation = str_to_lower(participation))

# Prelim plot
race %>%
  ggplot(aes(x = distance)) +
  geom_histogram(bins = 60) # Some races with distance = 0

race_half <- race %>% filter(distance > 0, distance < 100)

race_full <- race %>% 
  anti_join(race_half, by = "race_year_id")

ultra_rankings %>% 
  anti_join(race_half, by = "race_year_id") %>% 
  filter(!is.na(gender)) %>% 
  ggplot(aes(x = time, fill = gender, colour = gender)) +
  geom_density()