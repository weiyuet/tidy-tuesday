# Setup
library(tidyverse)
library(tidytext)
library(scales)

# Load data
chocolate <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

country_characteristics <- chocolate %>% 
  unnest_tokens(memorable_characteristic,
                most_memorable_characteristics, token = "regex",
                pattern = ",",
                to_lower = TRUE) %>% 
  mutate(memorable_characteristic = str_squish(memorable_characteristic)) %>%
  count(country_of_bean_origin, memorable_characteristic, sort = TRUE)

total_country_characteristics <- country_characteristics %>%
  group_by(country_of_bean_origin) %>%
  summarise(total = sum(n))

country_characteristics <- left_join(country_characteristics, total_country_characteristics, by = "country_of_bean_origin")

top_countries <- total_country_characteristics %>% 
  slice_max(n = 6, order_by = total) %>% 
  select(country_of_bean_origin)

# Filter top countries
country_characteristics <- country_characteristics %>% 
  filter(country_of_bean_origin %in% top_countries$country_of_bean_origin) %>%
  select(country_of_bean_origin, memorable_characteristic, n ,total)

# Plot most used descriptors for chocolate bars
country_characteristics %>% 
  group_by(country_of_bean_origin) %>% 
  slice_max(n, n = 15, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(country_of_bean_origin = as_factor(country_of_bean_origin),
    memorable_characteristic = reorder_within(memorable_characteristic, n, country_of_bean_origin)) %>% 
  ggplot(aes(x = n, y = memorable_characteristic)) +
  geom_col(colour = "gray10", fill = "gray35", show.legend = FALSE) +
  facet_wrap(~country_of_bean_origin, scales = "free") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  theme_classic() +
  labs(x = "", y = "",
       title = "Most-used Descriptors for Chocolate Bars",
       caption = "Data: flavorsofcacao.com\n #TidyTuesday2022 w3")

# Save png
ggsave("2022/w3/most-used-desciptors-chocolate-bars.png", width = 6, height = 8)