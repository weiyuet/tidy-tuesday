# Setup
library(tidyverse)

# Load data
yarn <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

yarn %>% 
  ggplot(aes(x = texture)) +
  geom_histogram(binwidth = 20, stat = "count")