#Setup
library(tidyverse)
library(scales)

#Load data
plastics <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv")

#Wrangle data
plastics_singapore <- plastics %>% filter(country == "Singapore")

#Prelim plot
plastics_singapore %>%
  mutate(parent_company = fct_reorder(parent_company, grand_total)) %>% 
  ggplot(aes(x = parent_company, y = grand_total)) +
  geom_col(colour = "gray10", fill = "gray35") +
  scale_y_continuous(limits = c(0, 40),
                     expand = c(0, 0)) +
  coord_flip() +
  theme_classic() +
  labs(x = "", y = "",
       title = "Plastic Pollution in Singapore",
       caption = "Source: Break Free from Plastic\n Graphic: @weiyuet #TidyTuesday 2021 w5")
