###################
# tidytuesday w45 #
###################

#### Setup ####
library(tidyverse)
library(scales)

#### Load data ####
state_stations <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

#### Wrangle ####
# Separate frequency and band, and recode frequency as numeric
state_stations <- state_stations %>% 
  separate(frequency, c("freq", "band"), sep = "\\s+") %>% 
  mutate(
    freq = as.numeric(freq),
    band = if_else(!is.na(band), band,
                   if_else(freq < 200, "FM", "AM"))
  )

#### Which states have the most radio stations? ####
state_stations %>% 
  count(state) %>% 
  mutate(state = fct_reorder(state, n)) %>% 
  ggplot(aes(x = n, 
             y = state)) +
  geom_col(
    colour = "gray10",
    fill = "gray35"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1500, 100),
    expand = c(0, 0)) +
  theme_classic() +
  labs(
    x = "",
    y = "",
    title = "Which States Have the Most Number of Radio Stations?",
    caption = "Data: Federal Communications Commission\n Graphic: @weiyuet | #TidyTuesday2022 w45")

#### Save image ####
ggsave("2022/w45/state-stations-number.png", width = 8, height = 5)