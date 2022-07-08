# Load libraries
library(tidyverse)
library(scales)

# Load data
rent <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

# Wrangle data
rent_small <- rent %>% 
  drop_na() %>% 
  select(year, nhood, city, price, beds) %>% 
  filter(city == "oakland" | city == "san francisco" | city == "san jose") %>% 
  arrange(year)

# Prelim plot
rent_small %>%
  ggplot(aes(x = year, y = price, colour = city)) +
  geom_point() +
  geom_jitter() +
  geom_smooth() +
  facet_wrap(~city) +
  scale_y_log10(labels = label_dollar(prefix = "USD", big.mark = ",", accuracy = 1)) +
  scale_x_continuous() +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90)) +
  scale_colour_brewer(type = "qual", palette = 6) +
  labs(x = "", y = "",
       title = "Rents in the Bay Area - Core Cities",
       caption = "Source: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018.\nChart: @weiyuet #TidyTuesday")

ggsave("2022/w27/rent-cupertino-frisco.png", width = 9, height = 6)
  