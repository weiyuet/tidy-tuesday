# Setup
library(tidyverse)
library(scales)

# Load data
sports <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv")

# Prelim plot of total revenue of men and women grouped by sports
sports %>%
      group_by(sports) %>%
      filter(total_rev_menwomen > 0) %>%
      summarise(., rev_menwomen = sum(total_rev_menwomen)) %>%
      arrange(., desc(rev_menwomen)) %>%
      slice_head(n = 7) %>%
      mutate(sports = fct_reorder(sports, rev_menwomen)) %>%
      ggplot(aes(x = rev_menwomen, y = sports)) +
      geom_col(colour = "gray10", fill = "gray35") +
      scale_x_continuous(
            expand = c(0, 0),
            limits = c(0, 30000000000),
            breaks = seq(0, 30000000000, 5000000000),
            labels = label_dollar(prefix = "$", big.mark = ",")
      ) +
      theme_classic() +
      theme(
            legend.position = "none",
            axis.text.y = element_text(angle = 0)
      ) +
      labs(
            x = "", y = "",
            title = "Top Seven Sports by Combined Revenue",
            caption = "Source: U.S. Department of Education EADA\nGraphic: @weiyuet | #TidyTuesday2022 w13"
      )

# Save png
ggsave("2022/w13/top-sports-combined-revenue.png", width = 7, height = 5)
