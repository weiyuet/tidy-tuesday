########################
# TidyTuesday 2023 w25 #
########################

#### Setup ####
library(tidyverse)
library(skimr)
library(lubridate)
library(scales)
library(patchwork)

#### Load Data ####
ufo_sightings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')
day_parts_map <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/day_parts_map.csv')

#### Explore Data ####
ufo_sightings %>% 
  skim() %>% 
  summary() # >96000 rows, 12 columns

range(ufo_sightings$reported_date_time) # 1925 to 2023

ufo_sightings %>% 
  count(city,
        sort = TRUE) # Mostly cities in USA

ufo_sightings %>% 
  count(state,
        sort = TRUE) # Mostly in CA

ufo_sightings %>% 
  count(country_code,
        sort = TRUE) # Mostly data from the USA

ufo_sightings %>% 
  count(shape,
        sort = TRUE) # top shapes reported are: light, circle, and triangle

#### Visualize ####
#   ____________________________________________________________________________
#   Have UFO sightings increased over time?                                 ####
p1 <- ufo_sightings %>% 
  group_by(year = year(reported_date_time)) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = year,
             y = count)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1925,
                                  2025,
                                  10)) +
  scale_y_continuous(breaks = seq(0,
                                  7000,
                                  1000),
                     labels = label_number(big.mark = ","),
                     expand = c(0.01, 0)) +
  labs(x = NULL,
       y = NULL,
       title = "The # of UFO sightings has increased over the years") +
  theme_classic()

#   ____________________________________________________________________________
#   What are the most common shapes reported?                               ####
p2 <- ufo_sightings %>%
  drop_na() %>% 
  count(shape,
        sort = TRUE) %>% 
  mutate(shape = fct_reorder(shape,
                             n)) %>% 
  arrange(n) %>% 
  ggplot(aes(x = shape,
             y = n)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,
                                  20000,
                                  2500),
                     labels = label_number(big.mark = ","),
                     expand = c(0.01, 0)) +
  coord_flip() +
  labs(x = NULL,
       y = NULL,
       title = "\"Light\" is the most commonly reported form of UFO sighting") +
  theme_classic()

#   ____________________________________________________________________________
#   Combine plots                                                           ####
p <- (p1/p2) + 
  plot_annotation(caption = "Data: National UFO Reporting Center, sunrise-sunset.org | #TidyTuesday 2023 w25")

#### Save Image ####
ggsave("2023/w25/ufo.png",
       width = 8,
       height = 6)