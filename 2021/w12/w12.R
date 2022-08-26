# Load libraries
library(tidyverse)
library(scales)
library(lubridate)
library(ggsci)

# Load data
games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

# Plot peak vs average number of players
games %>% 
  select(gamename, avg, peak) %>% 
  filter(gamename != "Cyberpunk 2077") %>% 
  slice_max(order_by = avg, n = 200) %>% 
  ggplot(aes(x = avg, y = peak, colour = gamename)) +
  geom_point(size = 0.9, show.legend = FALSE) +
  geom_smooth() +
  scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ",")) +
  scale_x_continuous(labels = label_number(accuracy = 1, big.mark = ",")) +
  scale_colour_jco() +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x = "", y = "",
       title = "Peak vs. Average Number of Players Online",
       subtitle = "y-axis: peak; x-axis: average",
       caption = "Source: Steam Charts\n#TidyTuesday")

#Save png
ggsave("2021/w12/peak-vs-average-number-of-players.png", width = 8, height = 6)

# Create a "year_month" variable with lubridate
games$year_month <- as_date(paste(games$year, match(games$month, month.name), 1, sep = "-"))

# Plot monthly player gains and losses for the top three most popular games
games %>% 
  select(gamename, year_month, gain) %>% 
  filter(gamename == "Dota 2" | 
           gamename == "PLAYERUNKNOWN'S BATTLEGROUNDS" | 
           gamename == "Counter-Strike: Global Offensive") %>% 
  ggplot(aes(x = year_month, y = gain, fill = gamename)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 100000, linetype = "dashed") +
  geom_hline(yintercept = 100000, linetype = "dashed") +
  facet_wrap(~gamename, scales = "free") +
  scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ",")) +
  scale_fill_jco() +
  theme_classic() +
  labs(x = "", y = "Average players gained/lost",
       title = "Players Gained or Lost per Month",
       subtitle = "Dashed lines at +/-100,000",
       caption = "Source: Steam Charts\n#TidyTuesday")

# Save png
ggsave("2021/w12/average-players-gained-lost.png", width = 10, height = 8)
