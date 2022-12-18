###################
# tidytuesday w50 #
###################

#### Setup ####
library(tidyverse)
library(scales)
library(tsibble)

#### Load data ####
state_retail <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/state_retail.csv',  col_types = "cciciiccc")
coverage_codes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/coverage_codes.csv')

#### Explore data ####
state_retail %>% 
  count(fips, state_abbr, sort = TRUE)

state_retail %>% 
  count(subsector, sort = TRUE)

#### Combine year and month, and filtered for overall USA data ####
state_retail <- state_retail %>% 
  mutate(time = make_yearmonth(year, month),
         across(starts_with("change_yoy"), as.numeric))

state_retail_time_series <- state_retail %>% 
  select(-fips, -naics, -year, -month) %>% 
  as_tsibble(key = c(state_abbr, subsector), index = time)

usa <- state_retail_time_series %>% 
  filter(state_abbr == "USA")

#### How did retail sales change between 2019 and 2022? ####
usa %>% 
  ggplot(aes(x = time, y = change_yoy)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  facet_wrap(~subsector, scales = "free_y") +
  theme_classic() +
  labs(x = "", y = "",
       title = "US Retail Sales Year-on-year Change",
       caption = "Data: US Census Bureau Monthly State Retails Sales\nGraphic: @weiyuet | #TidyTuesday2022 w50")

#### Save image ####
ggsave("2022/w50/us-retail-yoy.png", width = 10, height = 8)