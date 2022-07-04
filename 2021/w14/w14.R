# Loading libraries
library(tidyverse)
library(tidytext)
library(forcats)
library(ggridges)

# Loading data
allShades <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv")

# Wrangling data
# Selecting the 14 brands with the most foundations in the data set
top_brands <- allShades %>% 
  select(brand) %>% 
  count(brand) %>% 
  slice_max(order_by = n, n = 14)

# Filtering foundation names for lightness values

simplified_names <- allShades %>% 
  mutate(rounded = signif(lightness, digits = 1)) %>% 
  filter(!is.na(name)) %>% 
  filter(rounded %in% c(0.2, 0.4, 0.6, 0.8, 1.0)) %>% 
  select(name, rounded) %>% 
  unnest_tokens(word, name) %>%
  count(rounded, word, sort = TRUE)

# Counting total number of words for each lightness value
total_words <- simplified_names %>% 
  group_by(rounded) %>% 
  summarise(total = sum(n))

# Plotting foundations from top brands according to lightness
allShades %>% 
  filter(brand %in% top_brands$brand) %>% 
  ggplot(aes(lightness, brand, colour = hex)) +
  geom_jitter() +
  scale_colour_identity() +
  xlim(0, 1) +
  theme_light() +
  geom_vline(xintercept = 0.25, linetype = "dashed") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0.75, linetype = "dashed") +
  labs(x = "Lightness", y = "",
       title = "Foundations brands and their lightness",
       caption = "Source: The Pudding/Amber Thomas")

ggsave("2021/w14/foundations-vs-lightness.png", width = 6, height = 8)

# Plotting foundations distributions
allShades %>% 
  filter(brand %in% top_brands$brand) %>% 
  ggplot(aes(lightness, brand, fill = brand, group = brand)) +
  geom_density_ridges_gradient() +
  scale_fill_brewer(type = "qual", palette = 3) +
  xlim(0, 1) +
  theme_light() +
  geom_vline(xintercept = 0.25, linetype = "dashed") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0.75, linetype = "dashed") +
  theme(legend.position = "none") +
  labs(x = "Lightness", y = "",
       title = "Foundation shade distributions",
       caption = "Source: The Pudding/Amber Thomas")
  
ggsave("2021/w14/foundations-distributions.png", width = 6, height = 8)
