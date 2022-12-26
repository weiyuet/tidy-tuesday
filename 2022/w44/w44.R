###################
# tidytuesday w44 #
###################

#### Setup ####
library(tidyverse)
library(scales)

#### Load data ####
horror_movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')

#### Wrangle data ####
# Express budget and revenue in millions
# Create release year and release month
horror_movies <- horror_movies %>% 
  mutate(budget = budget/1000000,
         revenue = revenue/1000000,
         release_year = as.numeric(format(release_date, "%Y")),
         release_month = as.numeric(format(release_date, "%m")))

#### When are horror movies released? ####
horror_movies %>% 
  filter(release_year >= 2003) %>% 
  group_by(release_year, release_month) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = release_month, y = count)) +
  geom_line() +
  facet_wrap(~release_year,
             ncol = 4) +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb[1:12]) +
  theme_classic() +
  labs(x = "", y = "",
       title = "When Are Horror Movies Released?",
       subtitle = "Since the 2000s, more horror movies are released in October",
       caption = "Data: www.themoviedb.org\n Graphic: @weiyuet | #TidyTuesday2022 w44")

ggsave("2022/w44/when-are-horror-movies-released.png", width = 11, height = 7)
  