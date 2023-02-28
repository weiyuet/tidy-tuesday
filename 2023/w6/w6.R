#######################
# tidytuesday 2023 w6 #
#######################

#### Setup ####
library(tidyverse)
library(scales)
library(paletteer)

#### Load data ####
big_tech_stock_prices <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

#### Visualize ####
# The fab 5
stocks_selected <- c("AAPL", "AMZN", "GOOGL", "META", "MSFT")

big_tech_stock_prices %>%
  filter(stock_symbol %in% stocks_selected) %>% 
  ggplot(aes(x = date,
             y = adj_close,
             colour = stock_symbol)) +
  geom_step() +
  scale_x_date(labels = label_date_short(),
               date_breaks = "12 months") +
  scale_y_continuous(labels = label_number(prefix = "$")) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  labs(x = "",
       y = "",
       title = "Big Tech Stock Crash 2022 - The Fab 5, is the party over?",
       subtitle = "The Fab 5 generated 29.3% of the S&P500's gain by the end of the decade,\nbut accounted for 46.3% of the index's loss in 2022.",
       caption = "Data: Yahoo Finance via Kaggle (by Evan Gower)\nGraphic: @weiyuet | #TidyTuesday2023 w6") +
  theme_classic() +
  theme(legend.title = element_blank())

#### Save image ####
ggsave("2023/w6/big-tech-stock-prices.png", width = 7, height = 5)