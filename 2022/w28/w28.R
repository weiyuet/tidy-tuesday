# Setup
library(tidyverse)
library(scales)

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
