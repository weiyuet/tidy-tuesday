#Setup
library(tidyverse)
library(tidytext)
library(scales)
library(ggsci)

#Load data
rent <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv")

#Wrangle data
#The three core cities in Bay Area
cities_to_include <- c("oakland", "san jose", "san francisco")

rent_core_cities <- rent %>%
  drop_na() %>%
  select(year, nhood, city, price, beds) %>%
  filter(city %in% cities_to_include) %>%
  mutate(city = case_when(city == "oakland" ~ "Oakland",
                          city == "san jose" ~ "San Jose",
                          city == "san francisco" ~ "San Francisco",
                          TRUE ~ city)) %>% 
  arrange(year)

#Plot rent in three core cities in Bay Area
rent_core_cities %>%
  ggplot(aes(x = year, y = price, colour = city)) +
  geom_point() +
  geom_jitter() +
  geom_smooth() +
  facet_grid(~city) +
  scale_y_log10(
    labels = label_dollar(prefix = "USD ", big.mark = ",", accuracy = 1),
    limits = c(1000, 30000)
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(angle = 90)
  ) +
  scale_colour_jco() +
  labs(
    x = "", y = "",
    title = "Rents in the Bay Area: The Three Core Cities",
    caption = "Data: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018.\nGraphic: @weiyuet | #TidyTuesday2022 w27"
  )

#Save png
ggsave("2022/w27/rent-bay-area-core-cities.png", width = 9, height = 6)

#Load data
permits <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/sf_permits.csv")

#Wrangle data
#Calculate type and number of permits per street
permits_per_street <- permits %>%
  select(permit_type_definition, street_name, permit_number) %>%
  group_by(permit_type_definition, street_name) %>%
  tally()

#Plot top 20 streets with number of permits
permits_per_street %>%
  slice_max(order_by = n, n = 20) %>%
  mutate(street_name = reorder_within(street_name, n, permit_type_definition)) %>%
  ggplot(aes(x = n, y = street_name, fill = permit_type_definition)) +
  geom_col(colour = "gray10", show.legend = FALSE) +
  facet_wrap(~permit_type_definition, ncol = 2, scales = "free") +
  scale_x_continuous(labels = label_number(big.mark = ","),
                     expand = c(0, 0)) +
  scale_y_reordered() +
  theme_classic() +
  scale_fill_jco() +
  labs(
    x = "", y = "",
    title = "Construction Permit Types by Street",
    caption = "Data: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018.\n#TidyTuesday2022 w27"
  )

#Save png
ggsave("2022/w27/permits-per-street.png", width = 9, height = 6)
