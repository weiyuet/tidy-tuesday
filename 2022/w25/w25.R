# Setup
library(tidyverse)
library(skimr)
library(scales)
library(ggsci)
library(glue)

# Load data
# blackpast <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv")
# slave_routes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
# african_names <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

census_tidy <- census %>%
  filter(region == "USA Total") %>%
  pivot_longer(., total:black_slaves, names_to = "population_segment", values_to = "count") %>%
  filter(population_segment == "black" | population_segment == "black_free" | population_segment == "black_slaves") %>%
  mutate(case_when(population_segment == "black" ~ "Black",
                   population_segment == "black_free" ~ "Black Free",
                   population_segment == "black_slaves" ~ "Black Slaves"))

census_tidy %>% 
  ggplot(aes(x = year, y = count, colour = population_segment)) +
  geom_line(size = 1.1) +
  facet_grid(~population_segment) +
  scale_x_continuous(
    limits = c(min(census$year), max(census$year)),
    breaks = seq(min(census$year), max(census$year), 20)
  ) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  scale_colour_jco() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(
    x = "", y = "",
    colour = "",
    title = glue("Census of the United States {min(census$year)} - {max(census$year)}"),
    caption = "Data: U.S. Census Bureau\nGraphic: @weiyuet | #TidyTuesday2022 w25"
  )

ggsave("2022/w25/us-population-census-1800s.png", width = 8, height = 4.5)