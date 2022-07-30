# Load libraries
library(tidyverse)
library(scales)

# Load data
technology <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

# Wrangle data
# BCG vaccinations in Singapore
bcg_singapore <- technology %>% 
  filter(variable == "BCG" & iso3c == "SGP")

# Plot BCG vaccinations in Singapore over time
bcg_singapore %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(limits = c(80, 100),
    breaks = seq(80, 100, 2),
    labels = label_number(suffix = "%", big.mark = "", accuracy = 1)) +
  theme_classic() +
  labs(x = "", y = "",
       title = "BCG Vaccinations in Singapore",
       subtitle = "Percentage of children who received an immunization",
       caption = "Source: nber.org \nGraphic: @weiyuet #TidyTuesday")

# Save png
ggsave("2022/w29/bcg-vaccinations-singapore.png", width = 6, height = 4)

#Wrangle data
# BCG vaccinations in 10 ASEAN member nations
bcg_asean <- technology %>% 
  filter(variable == "BCG") %>% 
  filter(iso3c == "SGP" | iso3c == "MYS" | iso3c == "IDN" | 
           iso3c == "THA" | iso3c == "KHM" | iso3c == "LAO" | iso3c == "BRN" |
           iso3c == "MMR" | iso3c == "PHL" | iso3c == "VNM")

# Plot BCG vaccinations in 10 ASEAN member nations over time
bcg_asean %>% 
  ggplot(aes(x = year, y = value, colour = iso3c)) +
  geom_line(size = 1.1) +
  facet_wrap(~iso3c, ncol = 2) +
  scale_color_brewer(type = "qual", palette = 3) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "BCG Vaccinations in the 10 ASEAN Member Nations",
       subtitle = "Percentage of children who received an immunization",
       caption = "Source: nber.org \nGraphic: @weiyuet #TidyTuesday")

# Save png
ggsave("2022/w29/bcg-vaccinations-asean.png", width = 6, height = 8)
