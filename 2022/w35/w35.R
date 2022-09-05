# Setup
library(tidyverse)
library(scales)

# Load data
pell <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv")

# Wrangle data
# Top 10 for 2017
pell %>% group_by(YEAR) %>% 
  filter(YEAR == 2017) %>% 
  mutate(mean_award = mean(AWARD)) %>% 
  arrange(AWARD) %>% slice_tail(n = 10) %>% 
  ggplot(aes(x = NAME, y = AWARD)) +
  geom_col(colour = 'gray35') +
  geom_hline(yintercept = 5558692, colour = 'red', linetype = 'dashed') +
  scale_y_continuous(labels = label_number(prefix = 'US$', big.mark = ',')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        axis.text.y = element_text(angle = 35, hjust = 1)) +
  labs(x = '', y = '',
       title = 'Top 10 Institutions for Pell Awards 2017')

# Save png
ggsave('2022/w35/pell-top-10-2017.png', width = 7, height = 5)