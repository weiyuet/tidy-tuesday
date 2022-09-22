# Setup
library(tidyverse)
library(skimr)
library(scales)

# Load data
bigfoot <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv")

# Explore data
skim(bigfoot) %>% summary()

bigfoot %>%
  select_if(is_numeric) %>%
  skim()

# Plot Bigfoot sightings by season
bigfoot %>%
  count(season) %>%
  mutate(season = fct_reorder(season, n)) %>%
  ggplot(aes(x = season, y = n)) +
  geom_col(colour = "gray10", fill = "gray35") +
  geom_label(aes(x = season, y = n, label = round(n, 0)),
    hjust = 1,
    vjust = 0.5,
    colour = "white",
    fill = NA,
    label.size = NA,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = label_number(big.mark = ","),
    breaks = seq(0, 2000, 500),
    limits = c(0, 2000),
    expand = c(0, 0)
  ) +
  theme_classic() +
  labs(
    x = "", y = "",
    title = "Bigfoot Sightings by Season (1869 - 2021)",
    caption = "Data: Data.World\n Graphic: @weiyuet | #TidyTuesday2022 w37"
  )

# Save png
ggsave("2022/w37/bigfoot-sightings-season.png", width = 6, height = 4.5)

# Plot Bigfoot sightings by state
bigfoot %>%
  count(state) %>%
  mutate(state = fct_reorder(state, n)) %>%
  ggplot(aes(x = state, y = n)) +
  geom_col(colour = "gray10", fill = "gray35") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 600, 50),
    expand = c(0, 0)
  ) +
  theme_classic() +
  labs(
    x = "", y = "",
    title = "Bigfoot Sightings by State (1869 - 2021)",
    caption = "Data: Data.World\n Graphic: @weiyuet | #TidyTuesday2022 w37"
  )

# Save png
ggsave("2022/w37/bigfoot-sightings-state.png", width = 10, height = 8)
