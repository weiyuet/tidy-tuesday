###################
# tidytuesday w44 #
###################

#### Setup ####
library(tidyverse)
library(scales)

#### Load data ####
horror_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')

#### What is the relationship between revenue and budget ####
horror_movies %>% 
  filter(genre_names == "Horror") %>% 
  ggplot(aes(x = revenue, y = budget, size = popularity, colour = genre_names)) +
  geom_point() +
  scale_x_log10(labels = label_number(prefix = "$",
                                      big.mark = ",")) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Popularity Against Revenue of Horror Movies",
       subtitle = "Some relationship between popularity and revenue",
       caption = "Data: www.themoviedb.org | #TidyTuesday2022 w44")
  