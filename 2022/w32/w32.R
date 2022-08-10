# Load libraries
library(tidyverse)
library(scales)

# Load data
wheels <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv")

# Prelim plot
# Relationship between ride duration and height
wheels %>% 
  mutate(height_metres = height * 0.3048) %>% 
  ggplot(aes(x = ride_duration_minutes, y = height_metres)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(x = "Ride duration in minutes", y = "Height in metres",
       caption = "Source: Emil Hvitfeldt\n#TidyTuesday")

# Save png
ggsave("2022/w32/ride-duration-and-height.png", width = 6, height = 4)
