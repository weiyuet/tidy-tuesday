# Load libraries
library(tidyverse)
library(scales)
library(ggsci)

# Load data
chips <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-23/chips.csv')

# Wrangle data
# Plot process size in nm
chips %>% drop_na() %>% 
  ggplot(aes(x = year, y = process_size_nm)) +
  geom_smooth() + 
  scale_x_continuous(limits = c(2000, 2025),
                     breaks = seq(2000, 2025, 5)) +
  scale_y_continuous(limits = c(0, 200),
                     breaks = seq(0, 200, 20)) +
  theme_classic() +
  labs(x = '', y = 'Process size in nanometers',
       title = 'Process Size in Semiconductors',
       caption = 'Source: The CHIP Dataset \n#TidyTuesday')

# Save png

ggsave('2022/w34/process-size-nm.png', width = 6, height = 4.5)

# Plot transistor count Moore's Law
chips %>% drop_na() %>% 
  ggplot(aes(x = year, y = transistors_million)) +
  geom_smooth() +
  scale_x_continuous(limits = c(2000, 2025),
                     breaks = seq(2000, 2025, 5)) +
  scale_y_continuous(limits = c(0, 20000),
                     breaks = seq(0, 20000, 5000),
                     labels = label_number(big.mark = ",")) +
  theme_classic() +
  labs(x = '', y = 'Transistor count in millions',
       title = 'Transistor Count in Semiconductors aka Moore\'s Law',
       caption = 'Source: The CHIP Dataset \n#TidyTuesday')

# Save png
ggsave('2022/w34/transistor-count-millions.png', width = 6, height = 4.5)