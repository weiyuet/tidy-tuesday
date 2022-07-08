# Load libraries
library(forcats)
library(tidytext)
library(tidyverse)
library(tidytuesdayR)

# Load data
tt <- tt_load("2021-03-23")

# Wrangle data
# Extract US votes on each UN resolution
US_votes <- tt$unvotes %>% 
  filter(country_code == "US") %>% 
  select(rcid, vote)
colnames(US_votes) <- c("rcid", "US_vote")

# Combine US votes and resolution issues with "roll_calls" data set for important votes
important_votes <- tt$roll_calls %>% 
  filter(rcid %in% US_votes$rcid &
           importantvote == 1)
important_votes <- left_join(important_votes, tt$unvotes,
                             by = "rcid")
important_votes <- left_join(important_votes, US_votes,
                             by = "rcid")
important_votes <- important_votes %>% 
  filter(country != "United States") %>% 
  mutate(agree_with_US = vote == US_vote)
important_votes$agree_with_US <- as.factor(important_votes$agree_with_US)
levels(important_votes$agree_with_US)

levels(important_votes$agree_with_US) <- c("Votes in disagreement with US",
                                           "Votes in agreement with US")
important_votes <- left_join(important_votes,
                             tt$issues, by = "rcid")
important_votes

# Count number of votes for each country that matches with the US vote
agree_with_US_count <- important_votes %>% 
  group_by(agree_with_US) %>% 
  select(country, agree_with_US) %>% 
  count(agree_with_US, country, sort = TRUE)
agree_with_US_count$country <- as.factor(agree_with_US_count$country)

# Count number of votes same/different as the US vote on UN resolution in different categories
votes_by_issue <- important_votes %>% 
  filter(!is.na(issue)) %>% 
  select(date, agree_with_US, issue) %>% 
  group_by(date, issue) %>% 
  count(agree_with_US)

# Plot countries that agreed/disagreed with the most US votes
agree_with_US_count %>% 
  slice_head(n = 15) %>% 
  ggplot(aes(n, fct_reorder(country, n), fill = agree_with_US)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~agree_with_US, ncol = 2, scales = "free") +
  theme_light() +
  scale_fill_brewer(type = "qual", palette = 2, direction = -1) +
  labs(x = "Votes on important UN resolutions", y = "",
       title = "Votes on important United Nations (UN) resolutions",
       caption = "Source: Voeten, Erik, Data and Analyses of Voting in the UN General Assembly (July 17, 2012)")
  
ggsave("2021/w13/agree-disagree-with-US-UN-resolutions.png", width = 8, height = 6)

# Plot votes on different categories of UN resolutions over time
votes_by_issue %>% 
  ggplot(aes(x = date, y = n, group = agree_with_US, colour = agree_with_US)) +
  geom_line(size = 0.8) +
  facet_wrap(~issue, scales = "free") +
  theme_light() +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 12)) +
  scale_fill_brewer(type = "qual", palette = 2) +
  labs(x = "", y = "UN resolution votes",
       title = "Votes on UN resolutions over time by issue",
       subtitle = "Agreement/disagreement with US vote used to gauge consensus on issues",
       caption = "Source: Voeten, Erik, Data and Analyses of Voting in the UN General Assembly (July 17, 2012)",
       colour = NULL)

ggsave("2021/w13/votes-UN-resolutions-over-time.png", width = 8, height = 6)