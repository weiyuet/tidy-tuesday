# Setup
library(tidyverse)
library(scales)
library(lubridate)
library(janitor)

# Load data
hydro_waste <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv')

# Clean column names
hydro_waste <- hydro_waste %>% clean_names()

# Plot data
p <- map_data("world") %>% 
  as_tibble() %>% 
  filter(region != "Antarctica") %>% 
  mutate(region = case_when(
    region == "USA" ~ "United States",
    region == "UK" ~ "United Kingdom",
    TRUE ~ region)) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               colour = "black", size = 0.1, fill = "white") +
  geom_point(aes(x = lon_wwtp, y = lat_wwtp, colour = env_concern),
             data = hydro_waste %>% 
               mutate(env_concern = if_else(df < 10, "Yes", "No")),
             size = 0.1) +
  scale_colour_manual(values = c("gray80", "red"),
                      guide = guide_legend(override.aes = list(size = 1))) +
  theme_minimal() +
  theme(plot.background = element_rect(colour = "white", fill = "white")) +
  labs(x = "", y = "",
       title = "Waste Water Treatment Plants",
       subtitle = "Potential environmental concerns where plants show a dilution factor of less than 10",
       colour = "Dilution factor < 10?",
       caption = "Data: Ehalt Macedo, H., Lehner, B., Nicell, J., Grill, G., Li, J., Limtong, A., and Shakya, R.:\nDistribution and characteristics of wastewater treatment plants within the global river network,\nEarth Syst. Sci. Data, 14, 559–577,\nhttps://doi.org/10.5194/essd-14-559-2022, 2022.")
