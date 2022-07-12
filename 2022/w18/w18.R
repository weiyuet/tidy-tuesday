# Load libraries
library(tidyverse)
library(scales)

# Load data capacity
capacity <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')

# Plot total capacity
capacity %>% 
  ggplot(aes(x = year, y = total_gw, colour = type)) +
  geom_line() +
  geom_point(show.legend = "none") +
  scale_y_log10() +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.3)) +
  scale_colour_brewer(type = "qual", palette = 2) +
  labs(x = "", y = "Total GW",
       title = "Total Installed Capacity",
       caption = "Source: Berkeley Lab\nGraphic: @weiyuet #TidyTuesday")

# Save png
ggsave("2022/w18/total-capacity.png", width = 8, height = 6)

# Load data average cost
average_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')

# Wrangle data
# Change to long format
average_cost_long <- average_cost %>% 
  pivot_longer(gas_mwh:wind_mwh, names_to = "type", values_to = "average_cost")

# Plot average cost
average_cost_long %>% 
  ggplot(aes(x = year, y = average_cost, colour = type)) +
  geom_line() +
  geom_point(show.legend = "none") +
  scale_x_continuous(breaks = seq(2008, 2022, 2), expand = c(0, 0)) +
  scale_y_log10(labels = label_dollar(prefix = "$")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8)) +
  scale_colour_brewer(type = "qual", palette = 2 , labels = c("Gas MWh", "Solar MWh", "Wind MWh")) +
  labs(x = "", y = "",
       title = "Average Cost of ...",
       caption = "Source: Berkeley Lab\nGraphic: @weiyuet #TidyTuesday")

#Save png
ggsave("2022/w18/average-cost.png", width = 8, height = 6)