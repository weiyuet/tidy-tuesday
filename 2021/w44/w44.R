# Setup
library(tidyverse)
library(scales)
library(ggsci)
library(skimr)
library(lubridate)

# Load data
ultra_rankings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# Wrangle
race <- race %>% 
  mutate(participation = str_to_lower(participation))

# Visualize
race %>%
  ggplot(aes(x = distance)) +
  geom_histogram(binwidth = 1) # Some races with distance = 0

race_half <- race %>% filter(distance > 0, distance < 100)

race_full <- race %>% 
  anti_join(race_half, by = "race_year_id")

ultra_rankings %>% 
  anti_join(race_half, by = "race_year_id") %>%
  filter(!is.na(gender)) %>%
  filter(age > 10, age < 100) %>%
  ggplot(aes(x = age, fill = gender)) +
  geom_histogram(stat = "count", binwidth = 1, colour = "gray10") +
  scale_x_continuous(breaks = seq(0, 85, 5)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 7000, 1000),
                     labels = label_number(big.mark = ",")) +
  scale_fill_aaas() +
  theme_classic() +
  theme(legend.position = c(0.9, 0.5)) +
  labs(x = "", y = "",
       fill = "",
       title = "Most Ultra Trail Runners are Over 40 Years Old",
       caption = "Data: Benjamin Nowak, International Trail Running Association (ITRA)\nGraphic: @weiyuet | #TidyTuesday2021 w44")

# Save image
ggsave("2021/w44/ultra-runners-age-distribution.png", width = 8, height = 5)