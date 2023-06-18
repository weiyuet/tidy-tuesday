#######################
# TidyTuesday 2023 w5 #
#######################

#### Setup ####
library(tidyverse)
library(lubridate)
library(patchwork)

#### Load Data ####
cats_uk <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')

#### Explore Data ####
#   ____________________________________________________________________________
#   Exploratory plots                                                       ####
# Where are most of the cats in the data set?
p1 <- map_data("world", "uk") %>% 
  ggplot(aes(x = long,
             y= lat)) +
  geom_polygon(aes(group = group),
               fill = "gray50") +
  geom_point(aes(x = location_long,
                 y = location_lat),
             data = cats_uk,
             alpha = 0.25) +
  coord_map() +
  labs(x = "",
       y = "",
       title = "Most cats are located in the\nSouthwest of England") +
  theme_classic()

# How old are the cats?
p2 <- cats_uk_reference %>% 
  filter(!is.na(age_years)) %>% 
  ggplot(aes(age_years)) +
  geom_histogram(aes(fill = animal_sex),
                 binwidth = 1,
                 position = position_dodge(width = 0.8),
                 show.legend = FALSE) +
  scale_y_continuous(breaks = 0:10,
                     expand = c(0.01, 0)) +
  labs(x = "Age (years)",
       y = "",
       fill = "",
       title = "Most cats are below 5 years old") +
  theme_classic() +
  theme(panel.grid.minor.y = element_blank())

# How many prey do they catch?
p3 <- cats_uk_reference %>% 
  ggplot(aes(prey_p_month)) +
  geom_histogram(aes(fill = animal_sex),
                 binwidth = 1,
                 position = position_dodge()) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Number of prey caught per month",
       y = "",
       fill = "",
       title = "Most cats don't catch many prey") +
  theme_classic() +
  theme(legend.position = c(0.85, 0.65))

# Combine plots
p <- p1 | p2/p3

p + plot_annotation(caption = "Data: Movebank for Animal Tracking Data\n #TidyTuesday2023 w5")

#### Save Image ####
ggsave("2023/w5/pet-cats-uk.png",
       width = 8,
       height = 6)