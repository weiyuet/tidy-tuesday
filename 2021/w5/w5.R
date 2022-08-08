# Load libraries
library(tidyverse)
library(scales)

# Load data
plastics <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv")

# Wrangle data
plastics_singapore <- plastics %>% filter(country == "Singapore")

# Prelim plot
plastics_singapore %>% 
  ggplot(aes(x = parent_company, y = grand_total)) +
  geom_point() +
  theme_classic() +
  labs(x = "", y = "")
