####################
# tidytuesday 2023 #
####################

#### Setup ####
library(tidyverse)
library(scales)

#### Load data ####
big_tech_stock_prices <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

#### Wrangle ####

#### Visualize ####
big_tech_stock_prices %>% 
  ggplot(aes(x = date,
             y = adj_close,
             colour = stock_symbol)) +
  geom_step() +
  scale_x_date(labels = label_date_short(),
               date_breaks = "12 months") +
  scale_y_log10(labels = label_number(prefix = "$",
                                      big.mark = ""),
                limits = c(1, 1000)) +
  labs(x = "",
       y = "",
       title = "Big Tech Stock Prices",
       subtitle = "Adjusted close - after splits and dividend distributions",
       caption = "Data: Yahoo Finance via Kaggle (by Evan Gower)\nGraphic: @weiyuet | TidyTuesday2023 w6") +
  theme_classic() +
  theme(legend.title = element_blank())

#### Save image ####
ggsave("2023/w6/big-tech-stock-prices.png", width = 7, height = 5)