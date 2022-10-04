# Setup
library(tidyverse)
library(lubridate)
library(janitor)
library(tsibble)
library(scales)

# Load data
drought <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv")

# Wrangle
drought <- drought %>%
  clean_names() %>%
  mutate(date = date %>%
    str_remove("^d_") %>%
    ymd())

drought <- drought %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date, key = state)

# Visualize
# Extreme dry conditions
drought %>%
  group_by(state) %>%
  mutate(year = year(date)) %>%
  ggplot(aes(x = year, y = d0, fill = d0)) +
  geom_col() +
  facet_wrap(~state) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1895, 2025, 35)
  ) +
  scale_fill_gradient2(
    low = "white", mid = "gray", high = "darkred",
    midpoint = 50
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = "", y = "",
    fill = "",
    title = "Abnormally Dry Years in the 48 US States between 1895-2022",
    caption = "Data: US Drought Monitor\nGraphic: @weiyuet | #TidyTuesday2022 w24"
  )

ggsave("2022/w24/extreme-dry.png", width = 11, height = 8)
