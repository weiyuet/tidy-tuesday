# Setup
library(tidyverse)
library(scales)
library(glue)

# Load data
spiders <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

# Visualize
spiders %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_step() +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(1750, 2025, 20)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 1020, 100)) +
  theme_classic() +
  labs(x = "", y = "",
       title = glue("Number of Spider Species Discovered Between {min(spiders$year)} to {max(spiders$year)}"),
       subtitle = "The # of new species discovered is still increasing",
       caption = "Data: World Spider Database, Majer et al, 2015\nGraphic: @weiyuet | #TidyTuesday2021 w50")

# Save image
ggsave("2021/w50/number-of-spider-species-discovered.png", width = 8, height = 5)