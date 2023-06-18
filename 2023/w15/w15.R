########################
# TidyTuesday 2023 w15 #
########################

#### Setup ####
library(tidyverse)
library(scales)
library(paletteer)

#### Load Data ####
eggproduction  <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
cagefreepercentages <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/cage-free-percentages.csv')

#### Explore Data ####
eggproduction %>% 
  count(prod_type,
        sort = TRUE) # Only table eggs, and hatching eggs

eggproduction %>% 
  count(prod_process,
        sort = TRUE) # Three categories, all, cage-free (non-organic), and cage-free (organic)

range(eggproduction$observed_month) # Date from 2016 - 2021

#   ____________________________________________________________________________
#   Visualize                                                               ####
# How many organic, and non-organic eggs are produced?
prod_process_selected <- c("cage-free (non-organic)",
                           "cage-free (organic)")

eggproduction %>% 
  filter(prod_process %in% prod_process_selected) %>% 
  ggplot(aes(x = observed_month,
             y = n_eggs,
             colour = prod_process)) +
  geom_line() +
  scale_x_date(date_breaks = "3 months",
               labels = label_date_short()) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  labs(x = "",
       y = "",
       colour = "",
       title = "Egg Production in the USA (2016-2021)",
       subtitle = "The production of non-organic eggs still far outpaces organic eggs.",
       caption = "Data: US Egg Production Data\nGraphic: @weiyuet | #TidyTuesday2023 w15") +
  theme_classic() +
  theme(legend.position = c(0.2, 0.85))

#### Save Image ####
ggsave("2023/w15/egg-production.png",
       width = 7,
       height = 5)