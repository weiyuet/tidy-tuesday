########################
# TidyTuesday 2023 w22 #
########################

#### Setup ####
library(tidyverse)
library(paletteer)
library(patchwork)

#### Load Data ####
centenarians <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

#### Visualize ####
# When are centenarians born?
p1 <- centenarians %>% 
  count(gender,
        birth_month = month(birth_date,
                            label = TRUE)) %>% 
  ggplot(aes(x = birth_month,
             y = n)) +
  geom_col(aes(fill = gender),
           position = "dodge",
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 20)) +
  scale_fill_paletteer_d("ggsci::default_nejm") +
  labs(x = "Birth month",
       y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

# When do centenarians pass on?
p2 <- centenarians %>% 
  count(gender,
        death_month = month(death_date,
                            label = TRUE)) %>% 
  ggplot(aes(x = death_month,
             y = n)) +
  geom_col(aes(fill = gender),
           position = "dodge") +
  scale_fill_paletteer_d("ggsci::default_nejm") +
  labs(x = "Death month",
       y = "",
       fill = "Gender") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

p <- (p1 | p2) +
  plot_annotation(title = "More centenarians passed away in January compared to other months",
                  caption = "Data: Wikipedia List of the verified oldest people via frankiethull on GitHub\n#TidyTuesday2023 w22")

#### Save Image ####
ggsave("2023/w22/centenarians.png",
       width = 7,
       height = 5)