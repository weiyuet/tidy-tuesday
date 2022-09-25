# Setup
library(tidyverse)
library(scales)
library(ggsci)

wind <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
capacity <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')

# Load data average cost
average_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')

# Change to long format
average_cost_long <- average_cost %>% 
  pivot_longer(gas_mwh:wind_mwh, names_to = "type", values_to = "average_cost")

# Plot average cost
average_cost_long %>% 
  ggplot(aes(x = year, y = average_cost, colour = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(min(average_cost$year), max(average_cost$year), 1),
                     limits = c(min(average_cost$year), max(average_cost$year)),
                     expand = c(0, 0)) +
  scale_y_log10(limits = c(10, 300),
                labels = label_dollar(prefix = "$", accuracy = 1)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8)) +
  scale_colour_jco(labels = c("Gas MWh", "Solar MWh", "Wind MWh")) +
  labs(x = "", y = "",
       title = "Average Cost of ...",
       caption = "Data: Berkeley Lab\nGraphic: @weiyuet | #TidyTuesday2022 w18")

#Save png
ggsave("2022/w18/average-cost.png", width = 8, height = 4.5)