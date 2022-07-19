# Load libraries
library(tidyverse)
library(scales)

# Load data
sports <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

# Prelim plot of total revenue of men and women grouped by sports
sports %>% group_by(sports) %>%
  filter(total_rev_menwomen > 0) %>%
  summarise(., rev_menwomen = sum(total_rev_menwomen)) %>% 
  arrange(., desc(rev_menwomen)) %>% 
  slice_head(n = 7) %>% 
  ggplot(aes(x = sports, y = rev_menwomen, colour = sports)) +
  geom_point(size = 2) +
  scale_y_log10(labels = label_dollar(prefix = "$", big.mark = ",")) +
  scale_colour_discrete(type = "qual") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90)) +
  labs(x = "", y = "",
       title = "Top 7 Sports by Combined Revenue",
       caption = "#TidyTuesday")

# Save png
ggsave("2022/w13/top-sports-combined-revenue.png", width = 7, height = 5)