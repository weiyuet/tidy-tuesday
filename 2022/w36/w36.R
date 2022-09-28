# Setup
library(tidyverse)
library(scales)

# Load data
inventories <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz")
inventory_sets <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz")
sets <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz")

# Merge inventory_sets and sets
inventory_merged <- inventory_sets %>% left_join(sets, by = "set_num")

inventory_merged %>% 
  count(year, theme_id, wt = quantity, name = "quantity", sort = TRUE)

# Plot past LEGO sets still in sale today
range(inventory_merged$year)

inventory_merged %>% 
  count(set_num, name, year, wt = quantity, name = "quantity", sort = TRUE) %>% 
  count(year, wt = quantity, name = "quantity") %>% 
  ggplot(aes(x = year, y = quantity)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1965, 2025),
                     breaks = seq(1965, 2025, 10)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 600),
                     breaks = seq(0, 600, 100)) +
  theme_classic() +
  labs(x = "Year of set debut", y = "Inventory quantity",
       title = "LEGO Sets from 1964 are still in inventory",
       caption = "Data: rebrickable\n#TidyTuesday2022 w36")
  
# Plot
sets %>% 
  filter(year >= 1970) %>% 
  mutate(
    decade = (year %/% 10) * 10,
    decade = paste0(decade, "s")
  ) %>% 
  ggplot(aes(x = num_parts)) +
  geom_density(adjust = 1/2, fill = "gray80") +
  geom_vline(xintercept = 1000, linetype = 2) +
  facet_wrap(~decade, nrow = 2) +
  scale_x_log10() +
  scale_y_continuous(expand = expansion(add = c(0, 0.1))) +
  theme_classic() +
  labs(x = "# of parts (log scale)", y = "",
       title = "LEGO Sets with more than 1,000 parts are getting more popular",
       caption = "Data: rebrickable\n#TidyTuesday2022 w36")