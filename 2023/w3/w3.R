#######################
# Tidytuesday 2023 w3 #
#######################

#### Setup ####
library(tidyverse)

#### Load data ####
artists <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

#### Wrangle ####
artists <- artists %>% 
  mutate(across(c(artist_gender, artist_race),
                \(x) if_else(x == "NA", NA_character_, x)))

#### Explore Data ####
artists %>% 
  count(artist_name, sort = TRUE) # 25 is max count

artists %>% 
  count(artist_unique_id, sort = TRUE)

artists %>% 
  count(artist_nationality, sort = TRUE) # top 3 are American, French and British

artists %>% 
  count(artist_nationality_other, sort = TRUE) # top 3 are American, French and Other

artists %>% 
  count(artist_gender, sort = TRUE) # 2762 male, 342 female

artists %>% 
  count(artist_race, sort = TRUE) # 2936 White, 83 African American, 79 Asian

artists %>% 
  count(artist_race_nwi, sort = TRUE) # 2936 White, 226 Non-White

artists %>% 
  count(artist_ethnicity, sort = TRUE) # 2946 Not Hispanic, 158 Hispanic

artists %>% 
  filter(artist_nationality == "Japanese") %>% 
  count(artist_name, sort = TRUE) # Harunobu 14, Buson 7, Hokusai 6

#### Visualize ####
artists %>% 
  count(book,
        edition_number,
        artist_gender,
        year) %>% 
  ggplot(aes(x = year,
             y = n)) +
  geom_area(aes(fill = artist_gender)) +
  facet_wrap(vars(book,
                  ncol = 1)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  labs(x = "Year of publication",
       y = "# of works listed",
       fill = "Gender of artist",
       title = "More female artists listed since 1980s, but still dominated by male artists",
       caption = "Data: arthistory package data | #TidyTuesday2024 w4") +
  theme(strip.background = element_rect(colour = "black",
                                        fill = "white"),
        strip.text = element_text(colour = "black"),
        panel.grid.minor = element_blank(),
        legend.position = c(0.6, 0.7))

#### Save Image ####
ggsave("2023/w3/artists.png",
       width = 7,
       height = 5)