# Setup
library(tidyverse)
library(scales)
library(ggsci)

# Load data
plastics <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv")

# Wrangle Count of types of plastic discarded
plastics %>%
  select(year, hdpe, ldpe, pet, pp, ps, pvc) %>% 
  group_by(year) %>% 
  summarise(across(hdpe:pvc, sum, na.rm = TRUE)) %>% 
  pivot_longer(!year, names_to = "type", values_to = "amount") %>%
  mutate(type = fct_reorder(type, amount)) %>% 
  ggplot(aes(x = amount, y = type, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge", colour = "gray10") + 
  scale_x_continuous(labels = label_number(big.mark = ","),
                     expand = c(0, 0),
                     limits = c(0, 200000)) +
  scale_fill_aaas() +
  theme_classic() +
  theme(legend.position = c(0.9, 0.2)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "", y = "",
       fill = "",
       title = "PET is the most common type of plastic discarded globally",
       caption = "Source: Break Free from Plastic\n Graphic: @weiyuet #TidyTuesday2021 w5")

# Save image
ggsave("2021/w5/types-of-plastic-discarded.png", width = 8, height = 4.5)