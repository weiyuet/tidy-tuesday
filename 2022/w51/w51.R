########################
# tidytuesday 2022 w51 #
########################

#### Setup ####
library(tidyverse)

#### Load data ####
weather_forecasts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/weather_forecasts.csv')
cities <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/cities.csv')
outlook_meanings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/outlook_meanings.csv')

#### Wrangle ####
weather_forecasts <- weather_forecasts %>% 
  mutate(temp_diff = observed_temp - forecast_temp)

#### Visualize ####
weather_forecasts %>% 
  ggplot(aes(x = temp_diff,
             colour = high_or_low)) +
  geom_histogram(binwidth = 1,
                 position = "dodge") +
  facet_wrap(vars(high_or_low)) +
  labs(x = "Temperature difference (observed temp - forecast temp in F)",
       y = "",
       title = "Forecast for high temperatures seem to be more accurate",
       caption = "Data: USA National Weather Service\nGraphic: @weiyuet | #TidyTuesday2022 w51") +
  theme_classic() +
  theme(legend.position = "none")

#### Save image ####
ggsave("2022/w51/weather-forecast.png", width = 7, height = 5)