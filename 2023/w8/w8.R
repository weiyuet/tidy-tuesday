#######################
# tidytuesday 2023 w8 #
#######################

#### Setup ####
library(tidyverse)

#### Load data ####
bob_ross <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')

#### Visualize ####
bob_ross %>% 
  ggplot(aes(x = episode,
             y = num_colors)) +
  geom_line() +
  geom_hline(yintercept = 10.6,
             linetype = "dashed",
             colour = "red") +
  facet_wrap(vars(season)) +
  scale_x_continuous(breaks = 1:13) +
  labs(x = "",
       y = "",
       title = "Number of Colours used by Bob Ross in his Paintings in each Episode",
       subtitle = "Horizontal dashed line represents the average # of colours in all 13 episodes",
       caption = "Data: Bob Ross Paintings via Bob Ross Colors data package\nGraphic: @weiyuet | #TidyTuesday2023 w8") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 6))

#### Save image ####
ggsave("2023/w8/bob-ross-paintings.png", width = 7, height = 6)