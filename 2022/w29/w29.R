# Load libraries
library(tidyverse)
library(scales)

# Load data
technology <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

#Wrangle data
# BCG vaccinations in 10 ASEAN member nations
bcg_asean <- technology %>% 
  filter(variable == "BCG") %>% 
  filter(iso3c == "SGP" | iso3c == "MYS" | iso3c == "IDN" | 
           iso3c == "THA" | iso3c == "KHM" | iso3c == "LAO" | iso3c == "BRN" |
           iso3c == "MMR" | iso3c == "PHL" | iso3c == "VNM")

# Plot BCG vaccinations in 10 ASEAN member nations
bcg_asean %>% 
  ggplot(aes(x = year, y = value, colour = iso3c)) +
  geom_line(size = 1.05, colour = "gray 35") +
  facet_wrap(~iso3c, ncol = 2) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "BCG Vaccinations in the 10 ASEAN Member Nations",
       subtitle = "Percentage of children who received an immunization",
       caption = "Source: nber.org \nGraphic: @weiyuet #TidyTuesday")

# Save png
ggsave("2022/w29/bcg-vaccinations-asean.png", width = 6, height = 8)

# Wrangle data
# Pol3 vaccinations in 10 ASEAN nations
pol3_asean <- technology %>% 
  filter(variable == "Pol3") %>% 
  filter(iso3c == "SGP" | iso3c == "MYS" | iso3c == "IDN" | 
           iso3c == "THA" | iso3c == "KHM" | iso3c == "LAO" | iso3c == "BRN" |
           iso3c == "MMR" | iso3c == "PHL" | iso3c == "VNM")

# Plot Pol3 vaccinations in 10 ASEAN member nations
pol3_asean %>% 
  ggplot(aes(x = year, y = value, colour = iso3c)) +
  geom_line(size = 1.05, colour = "gray 35") +
  facet_wrap(~iso3c, ncol = 2) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Polio Vaccinations in the 10 ASEAN Member Nations",
       subtitle = "Percentage of children who received an immunization",
       caption = "Source: nber.org \nGraphic: @weiyuet #TidyTuesday")

# Save png
ggsave("2022/w29/pol3-vaccinations-asean.png", width = 6, height = 8)

technology %>% select(variable, label, group, category) %>% 
  filter(group == "Consumption") %>% 
  filter(category == "Communications") %>% 
  distinct(variable)

# Wrangle data
# Cell subscribers in 10 ASEAN member nations
cell_subsc_asean <- technology %>% 
  filter(variable == "cell_subsc") %>% 
  filter(iso3c == "SGP" | iso3c == "MYS" | iso3c == "IDN" | 
           iso3c == "THA" | iso3c == "KHM" | iso3c == "LAO" | iso3c == "BRN" |
           iso3c == "MMR" | iso3c == "PHL" | iso3c == "VNM")

# Plot Cell subscribers in 10 ASEAN member nations
cell_subsc_asean %>% 
  ggplot(aes(x = year, y = value, colour = iso3c)) +
  geom_line(size = 1.05, colour = "gray 35") +
  geom_hline(yintercept = 1000000, colour = "red", linetype = "dotted") +
  facet_wrap(~iso3c, ncol = 2) +
  scale_y_log10(labels = label_number(big.mark = ",")) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Cell subscribers in the 10 ASEAN Member Nations",
       subtitle = "Log scale on y-axis, and horizontal line at 1 million subscribers",
       caption = "Source: nber.org \nGraphic: @weiyuet #TidyTuesday")

# Save png
ggsave("2022/w29/cell-subscribers-asean.png", width = 6, height = 8)

# Wrangle data
# Internet users in 10 ASEAN member nations
internetuser_asean <- technology %>% 
  filter(variable == "internetuser") %>% 
  filter(iso3c == "SGP" | iso3c == "MYS" | iso3c == "IDN" | 
           iso3c == "THA" | iso3c == "KHM" | iso3c == "LAO" | iso3c == "BRN" |
           iso3c == "MMR" | iso3c == "PHL" | iso3c == "VNM")

# Plot Internet users in 10 ASEAN member nations
internetuser_asean %>% 
  ggplot(aes(x = year, y = value, colour = iso3c)) +
  geom_line(size = 1.05, colour = "gray 35") +
  geom_hline(yintercept = 1000000, colour = "red", linetype = "dotted") +
  facet_wrap(~iso3c, ncol = 2) +
  scale_y_log10(labels = label_number(big.mark = ",")) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Internet users in 10 ASEAN member nations",
       subtitle = "Log scale on y-axis, and horizontal line at 1 million users",
       caption = "Source: nber.org \nGraphic: @weiyuet #TidyTuesday")

# Save png
ggsave("2022/w29/internetuser-asean.png", width = 6, height = 8)

# Wrangle data
# Fixed telephones in 10 ASEAN member nations
telephone_asean <- technology %>% 
  filter(variable == "telephone") %>% 
  filter(iso3c == "SGP" | iso3c == "MYS" | iso3c == "IDN" | 
           iso3c == "THA" | iso3c == "KHM" | iso3c == "LAO" | iso3c == "BRN" |
           iso3c == "MMR" | iso3c == "PHL" | iso3c == "VNM")

# Plot Fixed telephones in 10 ASEAN member nations
telephone_asean %>% 
  ggplot(aes(x = year, y = value, colour = iso3c)) +
  geom_line(size = 1.05, colour = "gray 35") +
  geom_hline(yintercept = 100000, colour = "red", linetype = "dotted") +
  facet_wrap(~iso3c, ncol = 2) +
  scale_y_log10(labels = label_number(big.mark = ",")) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Fixed telephones in 10 ASEAN member nations",
       subtitle = "Log scale on y-axis, and horizontal line at 100,000 telephones",
       caption = "Source: nber.org \nGraphic: @weiyuet #TidyTuesday")

# Save png
ggsave("2022/w29/telephone-asean.png", width = 6, height = 8)
