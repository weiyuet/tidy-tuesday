# Setup
library(tidyverse)
library(scales)
library(ggsci)

# Load data
flights <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

# Wrangle data
# Create tidy data set with one column for flight type, and one column for number
tidy_flights_per_airport <- function(input_flight_type) {
  flights %>%
    select(FLT_DATE, APT_NAME, all_of(input_flight_type)) %>%
    mutate(flight_type = as.character(input_flight_type)) %>%
    rename('number_of_flights' = input_flight_type)
}

flight_types <- colnames(flights)[8:13]

# Apply function to flight type vector using purrr::map()
tidy_flights_list <- map(flight_types, tidy_flights_per_airport)

# Bind tidy version of flight type to row using purrr:map_df
tidy_flights <- map_df(tidy_flights_list, rbind)

# Top 6 airports by total flights
top_airports <- tidy_flights %>% 
  filter(flight_type == 'FLT_TOT_1') %>% 
  filter(FLT_DATE == max(FLT_DATE)) %>% 
  slice_max(order_by = number_of_flights, n = 6)

# Change 'flight_type' to factor with descriptive levels
tidy_flights$flight_type <- as_factor(tidy_flights$flight_type)
levels(tidy_flights$flight_type) <- c('Arrivals', 'Arrivals (Airport Operator)',
  'Departures', 'Departures (Airport Operator)', 'Total', 'Total (Airport Operator')

# Plot arrivals and departures for busiest airports
tidy_flights %>% 
  filter(APT_NAME %in% top_airports$APT_NAME) %>% 
  filter(flight_type %in% c('Arrivals', 'Departures')) %>% 
  ggplot(aes(x = APT_NAME, y = number_of_flights, fill = flight_type)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = label_number(big.mark = ',')) +
  scale_fill_nejm() +
  theme_classic() +
  theme(legend.position = 'bottom') +
  labs(x = '', y = '',
       fill = '',
       title = 'Arrivals and Departures of Busiest Airports in Europe',
       caption = 'Source: EUROCONTROL #TidyTuesday')

# Save png
ggsave('2022/w28/arrivals-departures-busiest-airports-europe.png', width = 7, height = 5)