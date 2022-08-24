# Load libraries
library(tidyverse)
library(scales)

# Load data
wheels <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv")

# Plot
# Height of ferris wheels and ride duration
wheels %>% 
  mutate(height_metres = height * 0.3048) %>% 
  ggplot(aes(x = ride_duration_minutes, y = height_metres)) +
  geom_point() +
  geom_smooth(colour = '#2086AE') +
  theme_classic() +
  labs(x = "Ride duration in minutes", y = "Height in metres",
       title = "Height of Ferris Wheels vs. Ride Duration",
       caption = "Source: Emil Hvitfeldt\nGraphic: @weiyuet #TidyTuesday")

# Save png
ggsave("2022/w32/height-and-ride-duration.png", width = 6, height = 4)

# Height of ferris wheels and date of open
wheels %>% 
  select(height, opened, country, number_of_cabins) %>% 
  ggplot(aes(x = opened, y = height, size = number_of_cabins)) +
  geom_point() +
  scale_x_date() +
  scale_y_continuous(limits = c(100, 700),
                     breaks = seq(100, 700, 50)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Year of open", y = "Height in metres",
       size = "Number of cabins",
       title = "Height of Ferris Wheels vs. Year of Open",
       caption = "Source: Emil Hvitfeldt\nGraphic: @weiyuet #TidyTuesday")

# Save png
ggsave("2022/w32/height-and-date-of-open.png", width = 6, height = 6)