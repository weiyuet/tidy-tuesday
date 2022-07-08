# Load libraries
library(tidyverse)
library(scales)

# Load data
rent <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

# Wrangle data
rent_small <- rent %>% 
  drop_na() %>% 
  select(year, nhood, city, price, beds) %>% 
  filter(city == "cupertino" | city == "san francisco") %>% 
  arrange(year)

# Prelim plot
rent_small %>%
  ggplot(aes(x = year, y = price, colour = city)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~city) +
  scale_y_log10(labels = label_dollar(big.mark = ",", accuracy = 1)) +
  scale_x_continuous() +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 35)) +
  scale_colour_brewer(type = "qual", palette = 6) +
  labs(x = "", y = "",
       title = "Rents",
       caption = "#TidyTuesday")

ggsave("2022/w27/rent-cupertino.png", width = 6, height = 6)
  