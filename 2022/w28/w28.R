# Load libraries
library(tidyverse)
library(scales)

# Load data
flights <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

# Wrangle data
flights %>% select(YEAR, APT_NAME, FLT_TOT_1) %>%
  group_by(YEAR) %>% 
  arrange(desc(FLT_TOT_1)) %>%
  slice_max(FLT_TOT_1, n = 15)
