########################
# TidyTuesday 2023 w15 #
########################

#### Setup ####
library(tidyverse)

#### Load Data ####
eggproduction  <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
cagefreepercentages <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/cage-free-percentages.csv')

#### Explore Data ####
eggproduction %>% 
  count(prod_type, sort = TRUE) # Only table eggs, and hatching eggs

eggproduction %>% 
  count(prod_process, sort = TRUE) # Three categories, all, cage-free (non-organic), and cage-free (organic)

range(eggproduction$observed_month) # Date from 2016 - 2021

#### Visualize ####
prod_process_selected <- c("cage-free (non-organic)", "cage-free (organic)")

eggproduction %>% 
  filter(prod_process %in% prod_process_selected) %>% 
  ggplot(aes(x = observed_month,
             y = n_eggs,
             colour = prod_process)) +
  geom_line()