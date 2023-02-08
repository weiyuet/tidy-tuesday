#######################
# tidytuesday 2023 w6 #
#######################

#### Setup ####
library(tidyverse)
library(scales)
library(paletteer)
library(patchwork)

#### Load data ####
big_tech_stock_prices <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

#### Visualize ####
# FAANG stocks
stocks_selected <- c("AAPL", "AMZN", "GOOGL", "META", "NFLX")

p1 <- big_tech_stock_prices %>%
  filter(stock_symbol %in% stocks_selected) %>% 
  ggplot(aes(x = date,
             y = adj_close,
             colour = stock_symbol)) +
  geom_step() +
  scale_x_date(labels = label_date_short(),
               date_breaks = "12 months") +
  scale_y_log10(labels = label_number(prefix = "$",
                                      big.mark = ""),
                limits = c(1, 1000)) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  labs(x = "",
       y = "",
       title = "FAANG Stocks") +
  theme_classic() +
  theme(legend.title = element_blank())

# non-FAANG stocks
stocks_selected <- c("ADBE", "CRM", "CSCO", "IBM", "INTC", "MSFT", "NDVA", "ORCL", "TSLA")

p2 <- big_tech_stock_prices %>%
  filter(stock_symbol %in% stocks_selected) %>% 
  ggplot(aes(x = date,
             y = adj_close,
             colour = stock_symbol)) +
  geom_step() +
  scale_x_date(labels = label_date_short(),
               date_breaks = "12 months") +
  scale_y_log10(labels = label_number(prefix = "$",
                                      big.mark = ""),
                limits = c(1, 1000)) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  labs(x = "",
       y = "",
       title = "Non-FAANG Stocks") +
  theme_classic() +
  theme(legend.title = element_blank())

p <- p1 / p2
p + plot_annotation(title = "Big Tech Stock Crash 2022",
                    subtitle = "Adjusted close - after splits and dividend distributions",
                    caption = "Data: Yahoo Finance via Kaggle (by Evan Gower)\nGraphic: @weiyuet | TidyTuesday2023 w6")

#### Save image ####
ggsave("2023/w6/big-tech-stock-prices.png", width = 7, height = 7)