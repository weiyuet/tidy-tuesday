#######################
# tidytuesday 2023 w2 #
#######################

#### Setup ####
library(tidyverse)

#### Load data ####
feederwatch <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
site_data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

#### Wrangle ####
# Filter for 2021 in site data
site_data_2021 <- site_data %>% 
  filter(proj_period_id == "PFW_2021")

# Join 2021 observation and site data
obs_site_2021 <- feederwatch %>% 
  left_join(site_data_2021, by = "loc_id")

#### Visualize ####
# Does housing density co-relate with higher observations?
obs_site_2021 %>% 
  mutate(housing_density = factor(housing_density)) %>% 
  count(loc_id,
        housing_density) %>% 
  ggplot(aes(n)) +
  geom_histogram(aes(y = after_stat(density),
                     fill = housing_density),
                 binwidth = 1) +
  facet_grid(row = vars(housing_density)) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom") +
  labs(x = "",
       y = "",
       fill = "Housing Density of Observation Sites",
       caption = "Data: Project FeederWatch | #TidyTuesday2023 w2")