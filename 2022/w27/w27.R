# Load libraries
library(tidyverse)
library(scales)
library(ggsci)

# Load data
rent <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

# Wrangle data
# The three core cities in the Bay Area
rent_core_cities <- rent %>% 
  drop_na() %>% 
  select(year, nhood, city, price, beds) %>% 
  filter(city == "oakland" | city == "san jose" | city == "san francisco") %>% 
  arrange(year)

# Prelim plot
rent_core_cities %>%
  ggplot(aes(x = year, y = price, colour = city)) +
  geom_point() +
  geom_jitter() +
  geom_smooth() +
  facet_wrap(~city) +
  scale_y_log10(labels = label_dollar(prefix = "USD ", big.mark = ",", accuracy = 1),
                limits = c(1000, 30000)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90)) +
  scale_colour_jco() +
  labs(x = "", y = "",
       title = "Rents in the Bay Area: The Three Core Cities",
       caption = "Source: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018.\nGraphic: @weiyuet #TidyTuesday")

ggsave("2022/w27/rent-bay-area-core-cities.png", width = 9, height = 6)
  