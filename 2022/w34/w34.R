# Load libraries
library(tidyverse)
library(scales)
library(patchwork)
library(ggsci)

# Load data
chips <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-23/chips.csv')

# Wrangle data
# Plot process size in nm
p1 <- chips %>% drop_na() %>% 
  ggplot(aes(x = year, y = process_size_nm)) +
  geom_smooth(colour = '#27408B') + 
  scale_x_continuous(limits = c(2000, 2021),
                     breaks = seq(2000, 2021, 1)) +
  scale_y_continuous(limits = c(0, 200),
                     breaks = seq(0, 200, 25)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = '', y = '',
       title = 'Process Size in Semiconductors',
       subtitle = 'y-axis: Process size in nanometers')

# Plot transistor count Moore's Law
p2 <- chips %>% drop_na() %>% 
  ggplot(aes(x = year, y = transistors_million)) +
  geom_smooth(colour = '#27408B') +
  scale_x_continuous(limits = c(2000, 2021),
                     breaks = seq(2000, 2021, 1)) +
  scale_y_continuous(limits = c(0, 20000),
                     breaks = seq(0, 20000, 5000),
                     labels = label_number(big.mark = ",")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = '', y = '',
       title = 'Transistor Count in Semiconductors aka Moore\'s Law',
       subtitle = 'y-axis: Transistor count in millions')

# Combined plot
p <- p1 / p2
p + plot_annotation(caption = 'Source: The CHIP Dataset #TidyTuesday')

# Save png
ggsave('2022/w34/process-size-and-transistor-count.png', width = 6, height = 6)