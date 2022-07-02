# Loading libraries
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(forcats)

# Loading data
tt <- tt_load("2021-04-20")

# Wrangling data
# Selecting films in data set, creating a numeric variable for film duration called "runtime", creating a numeric variable for year added
netflix_movies <- tt$netflix_titles %>% 
  filter(type == "Movie") %>% 
  mutate(runtime = as.numeric(str_sub(duration, end = -5))) %>% 
  mutate(year_added = as.numeric(str_sub(date_added, start = -4)))

# Creating vector of MPA film ratings
MPA_ratings <- c("G", "PG", "PG-13", "R", "NC-17")

# Creating list of top categories ("listed in") on Netflix
top_listings <- tt$netflix_titles %>% 
  separate_rows(listed_in, sep = ", ") %>% 
  count(listed_in, sort = TRUE) %>% 
  select(listed_in) %>% 
  head()

# Counting occurrences of words in the descriptions of top categories
top_listing_words <- tt$netflix_titles %>% 
  separate_rows(listed_in, sep = ", ") %>% 
  filter(listed_in %in% top_listings$listed_in) %>% 
  select(listed_in, description) %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(listed_in, word, sort = TRUE)

# Counting total number of words in the description of top categories
total_words <- top_listing_words %>% 
  group_by(listed_in) %>% 
  summarise(total = sum(n))

# Adding word totals to individual word counts
top_listing_words <- left_join(top_listing_words, total_words, by = "listed_in")

# Adding tf-idf to these words counts
top_listing_words <- top_listing_words %>% 
  bind_tf_idf(word, listed_in, n)

# Plotting distributions of film length according to MPA rating
netflix_movies %>% 
  filter(type == "Movie" & !is.na(rating)) %>%
  filter(rating %in% MPA_ratings) %>% 
  mutate(rating = factor(rating, levels = rev(MPA_ratings))) %>% 
  
  ggplot(aes(x = rating, y = runtime, fill = rating)) +
  geom_violin() +
  geom_hline(yintercept = 90, linetype = 2) +
  coord_flip() +
  theme_light() +
  scale_fill_viridis_d() +
  theme(legend.position = "none") +
  labs(x = "", y = "Film duration (minutes)",
       title = "Films aimed at younger audiences tend to be shorter",
       subtitle = "Dashed line at 90 minutes",
       caption = "Source: Kaggle via Shivam Bansal")

ggsave("2021/w17/films-for-younger-audiences.png", width = 7, height = 5)

# Plotting keywords used in Netflix descriptions
top_listing_words %>% 
  slice_max(tf_idf, n = 20) %>% 
  
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = listed_in)) +
  geom_col() +
  theme_light() +
  scale_fill_discrete() +
  labs(x = "Term frequency-inverse document frequency (tf-idf)",
       y = "",
       fill = "Netflix categories",
       title = "Keywords in Netflix descriptions",
       subtitle = "Words that appear often in these categories, but not others",
       caption = "Source: Kaggle via Shivam Bansal")

ggsave("2021/w17/keywords-in-Netflix-descriptions.png", width = 7, height = 5)