# Setup
library(tidyverse)
library(scales)

# Load data
pell <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv")

# Wrangle data
# Top 10 for 2017
pell %>%
      group_by(YEAR) %>%
      filter(YEAR == 2017) %>%
      arrange(AWARD) %>%
      slice_tail(n = 10) %>%
      mutate(NAME = fct_reorder(NAME, AWARD)) %>%
      ggplot(aes(x = NAME, y = AWARD)) +
      geom_col(colour = "gray10", fill = "gray35") +
      scale_y_continuous(
            expand = c(0, 0),
            labels = label_number(prefix = "US$", big.mark = ",")
      ) +
      coord_flip() +
      theme_classic() +
      theme(
            axis.text.x = element_text(angle = 0, hjust = 1),
            axis.text.y = element_text(angle = 0, hjust = 1)
      ) +
      labs(
            x = "", y = "",
            title = "Top 10 Institutions for Pell Grants 2017",
            caption = "Data: US Dept of Education\n Graphic: @weiyuet | #TidyTuesday2022 w35"
      )

# Save png
ggsave("2022/w35/pell-top-10-2017.png", width = 7, height = 5)
