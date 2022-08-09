# Load libraries
library(tidyverse)
library(scales)

# Load data
wheels <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv")

# Prelim plot
wheels %>% 
  ggplot(aes(x = ride_duration_minutes, y = height)) +
  geom_point() +
  geom_smooth() +
  theme_classic()
