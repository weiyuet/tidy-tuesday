########################
# TidyTuesday 2023 w28 #
########################

#### Setup ####
library(tidyverse)
library(janitor)

#### Load Data ####
global_temps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')
nh_temps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv')
sh_temps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv')
zonann_temps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv')

#### Wrangle ####
# Column names to lower case
global_temps <- global_temps %>% 
  clean_names()

nh_temps <- nh_temps %>% 
  clean_names()

sh_temps <- sh_temps %>% 
  clean_names()

zonann_temps <- zonann_temps %>% 
  clean_names()

#### Visualize ####
lats <- c("equ_24n", "x24n_44n", "x44n_64n", "x64n_90n", "x24s_equ", "x44s_24s", "x64s_44s", "x90s_64s")

p <- zonann_temps %>% 
  select(year,
         all_of(lats)) %>% 
  pivot_longer(-year,
               names_to = "lat") %>% 
  mutate(
    hemi_sphere = if_else(str_detect(lat, "n"),
                          "Norther Hemisphere",
                          "Southern Hemisphere"),
    lat = factor(lat,
                 levels = lats,
                 labels = rep(c("0-24", "24-44", "44-64", "64-90"), 2))) %>% 
  ggplot(aes(x = year,
             y = value)) +
  geom_hline(yintercept = 0,
             colour = "gray70") +
  geom_line(aes(colour = lat)) +
  facet_wrap(vars(hemi_sphere)) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = NULL,
       y = NULL,
       colour = "Lattitude",
       title = "Higher lattitudes are more sensitive to climate change",
       caption = "Data: NASA GISS Surface Temperature Analysis (GISTEMP v4)\n#TidyTuesday 2023 w28") +
  theme(legend.position = "top",
        strip.background = element_rect(fill = "white",
                                        colour = "gray50"),
        strip.text = element_text(colour = "black"))

#### Save Image ####
ggsave("2023/w28/global-surface-temperatures.png",
       p,
       width = 7,
       height = 5)