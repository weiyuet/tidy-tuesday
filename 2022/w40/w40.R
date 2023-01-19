########################
# tidytuesday 2022 w40 #
########################

#### Setup ####
library(tidyverse)
library(skimr)
library(lubridate)

#### Load data ####
product_hunt <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')

#### Wrangle ####
product_hunt <- product_hunt %>% 
  mutate(release_date = date(release_date),
         last_updated_date = date(last_updated))

#### Visualize ####
product_hunt %>% 
  ggplot(aes(x = release_date)) +
  geom_histogram(binwidth = 20)

product_hunt %>% 
  ggplot(aes(x = upvotes)) +
  geom_histogram() +
  scale_x_log10()