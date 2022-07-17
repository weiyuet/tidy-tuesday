# Load libraries
library(tidyverse)
library(scales)

# Load data
sports <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

# Prelim plot
sports %>% 
  filter(sports == "Basketball" | classification_name == "NCAA Division I") %>% 
  group_by(year) %>%
  sports$mean <- mutate(mean(rev_men)) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_point()
