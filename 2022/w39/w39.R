# Setup
library(tidyverse)
library(scales)
library(skimr)

# Load data
artists <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')

# Visualize
artists %>%
  group_by(state, type) %>% 
  summarize(artists_n = sum(artists_n, na.rm = TRUE), .groups = "drop_last") %>% 
  mutate(state_share = artists_n / sum(artists_n)) %>% 
  slice_max(state_share, n = 3) %>% 
  ungroup() %>% 
  ggplot(aes(x = state_share, y = state)) +
  geom_col() +
  facet_wrap(~type, scales = "free_y")
  