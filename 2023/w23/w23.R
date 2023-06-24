########################
# TidyTuesday 2023 w23 #
########################

#### Setup ####
library(tidyverse)
library(skimr)

#### Load Data ####
owid_energy <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')

#### Explore Data ####
owid_energy %>% 
  skim() %>% 
  summary() # Almost 22,000 rows, 129 columns

owid_energy %>% 
  count(country,
        sort = TRUE) # 306 countries/locations

range(owid_energy$year) # Between 1900 to 2022

column_names <- names(owid_energy)
print(column_names) # 129 columns

#### Wrangle ####
# Create function to change data to tidy format
tidy_energy <- function(type){
  owid_energy %>% 
    select(country,
           iso_code,
           year,
           ends_with(type)) %>% 
    pivot_longer(ends_with(type),
                 names_to = "energy",
                 names_pattern = paste0("(.*)",
                                        type))
}

#### Visualize ####
#   ____________________________________________________________________________
#   Exploratory plots                                                       ####
tidy_energy("_elec_per_capita") %>% 
  filter(year >= 1960) %>% 
  ggplot(aes(x = year,
             y = value)) +
  geom_line(aes(colour = country),
            show.legend = FALSE) +
  facet_wrap(vars(energy),
             scales = "free") +
  labs(x = "",
       y = "") +
  theme_classic()

# Create a vector containing different categories of energy
category <- c("coal",
          "oil",
          "gas",
          "hydro",
          "nuclear",
          "biofuel",
          "solar",
          "wind")

tidy_energy("_share_energy") %>% 
  filter(year >= 1965,
         energy %in% category) %>% 
  mutate(energy = factor(energy,
                         levels = category)) %>% 
  ggplot(aes(x = year,
             y = value)) +
  geom_line(aes(colour = country),
            show.legend = FALSE) +
  facet_wrap(vars(energy),
             nrow = 2) +
  labs(x = "",
       y = "Share of primary energy consumption (percent)") +
  theme_classic()

#   ____________________________________________________________________________
#   Main plot - Is the world moving away from hydrocarbons?                 ####
tidy_energy("_share_elec") %>% 
  filter(year >= 1990,
         energy %in% category) %>% 
  mutate(energy = factor(energy, levels = category)) %>% 
  ggplot(aes(x = year,
             y = value)) +
  geom_line(aes(group = country),
            colour = "gray70",
            show.legend = FALSE) +
  geom_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(vars(energy),
             nrow = 2) +
  labs(x = "",
       y = "Share of electricty generation (percent)",
       title = "Electricity generation is transiting away from hydrocarbons very slowly",
       subtitle = "Blue line shows the overall linear trend; light gray lines are individual countries",
       caption = "Data: Our World in Data (https://github.com/owid/energy-data)\nGraphic: @weiyuet | #TidyTuesday2023 w23") +
  theme_classic()

#### Save Image ####
ggsave("2023/w23/electricity-generation-share.png",
       width = 8,
       height = 5)