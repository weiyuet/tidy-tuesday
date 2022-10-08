# Setup
library(tidyverse)
library(janitor)
library(scales)
library(ggsci)

# Load data
indoor_pollution <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')

# Change column names to lower case
indoor_pollution <- indoor_pollution %>% 
  clean_names()

# Change country and percentage deaths column name
indoor_pollution <-  indoor_pollution %>% 
  rename(country = entity, 
         percentage_deaths = deaths_cause_all_causes_risk_household_air_pollution_from_solid_fuels_sex_both_age_age_standardized_percent)

# Visualize
indoor_pollution %>% 
  filter(country == "Afghanistan") %>%
  ggplot(aes(x = year, y = percentage_deaths)) +
  geom_line()

locations <- c("African Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region", "World", "G20", "OECD Countries")

indoor_pollution %>% 
  filter(country %in% locations) %>% 
  ggplot(aes(x = year, y = percentage_deaths, colour = country)) +
  geom_line() +
  scale_x_continuous(expand = c(0.01, 0),
                     limits = c(1990, 2020),
                     breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(labels = label_number(suffix = "%",
                                           decimal.mark = ".",
                                           accuracy = 0.1),
                     breaks = seq(0, 20, 2)) +
  scale_colour_jco() +
  theme_classic() +
  theme(legend.position = "top") +
  guides(guide_legend(nrow = 2)) +
  labs(x = "", y = "",
       colour = "",
       title = "Percentage of Deaths Caused by Indoor Pollution",
       subtitle = "The Western Pacific Region saw the best improvements",
       caption = "Data: Our World in Data\nGraphic: @weiyuet | #TidyTuesday2022 w15")

# Save image
ggsave("2022/w15/indoor-pollution.png", width = 7, height = 5)