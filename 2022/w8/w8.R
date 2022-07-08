# Load libraries
library(tidyverse)
library(rnaturalearth)
library(sf)
library(cowplot)

# Load data
freedom <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv")
head(freedom);names(freedom)

# Wrangle data
freedom1 <- freedom %>% 
  mutate(country=case_when(country=="Bolivia (Plurinational State of)"~"Bolivia",
                           country=="Côte d’Ivoire"~"Ivory Coast",
                           country=="United Kingdom of Great Britain and Northern Ireland"~"United Kingdom",
                           country=="Congo"~"Republic of Congo",
                           country=="Russian Federation"~"Russia",
                           country=="Brunei Darussalam"~"Brunei",
                           country=="Venezuela (Bolivarian Republic of)"~"Venezuela",
                           country=="Lao People's Democratic Republic"~"Laos",
                           country=="Viet Nam"~"Vietnam",
                           country=="Bahamas"~"The Bahamas",
                           country=="Guinea-Bissau"~"Guinea Bissau",
                           country=="Serbia"~"Republic of Serbia",
                           country=="North Macedonia"~"Macedonia",
                           country=="Czechia"~"Czech Republic",
                           country=="Timor-Leste"~"East Timor",
                           country=="Syrian Arab Republic"~"Syria",
                           country=="Iran (Islamic Republic of)"~"Iran",
                           country=="Republic of Moldova"~"Moldova",
                           country=="Democratic People's Republic of Korea"~"North Korea",
                           country=="Republic of Korea"~"South Korea",
                           TRUE ~ country))

world <- ne_countries(type = "countries")

w_countries <- world%>%
  as.data.frame()%>%
  count(sovereignt)%>%
  select(-n)%>%
  unlist()
my_countries <- freedom1%>%
  count(country)%>%
  select(-n)%>%
  unlist()
setdiff(w_countries,my_countries)

world1 <- ne_countries(scale = 110,type = "countries",
                       returnclass = "sf")
world12 <- world1%>%
  as.data.frame()%>%
  select(country = sovereignt,geometry)%>%
  filter(!country == "Antarctica")
freedom12 <- freedom1%>%
  left_join(world12,by = "country")

# Plot
plot <- ggplot()+
  geom_sf(data = world12,mapping = aes(geometry = geometry),size = 0.05) +
  geom_sf(data = freedom12,mapping = aes(geometry = geometry,fill = factor(Status)),size = 0.1)+
  scale_fill_viridis_d(labels = c("Free", "Not Free", "Partially Free")) +
  labs(fill = "Status", title = "Freedom in the World 1995-2020",
       caption = "Source: Freedom House and the United Nations\n #TidyTuesday") +
  coord_sf() +
  facet_wrap(vars(year)) +
  theme_map() +
  theme(text = element_text(family = "Arial"),
        plot.title.position = "panel",
        plot.title = element_text(size = 14),
        plot.caption = element_text(size = 4),
        plot.background = element_rect(fill = "gray92",colour = "gray92"),
        panel.background = element_rect(fill = "gray92",colour = "gray92"),
        legend.position = c(0.05, -0.055),
        legend.direction = "horizontal",
        legend.box.background = element_rect(fill = "gray92",colour = "gray92"),
        legend.background = element_rect(fill = "gray92",colour = "gray92"),
        legend.key.size = unit(0.2, 'cm'), #change legend key size
        legend.key.height = unit(0.2, 'cm'), #change legend key height
        legend.key.width = unit(0.2, 'cm'), #change legend key width
        legend.title = element_text(size = 4), #change legend title font size
        legend.text = element_text(size = 4),
        strip.background = element_blank(),
        strip.text = element_text(size = 4))

# Add notations
ggdraw()+
  draw_plot(plot)+
  draw_label("Political rights and civil liberties around the world deteriorated to their\n lowest point in more than a decade in 2017, extending a period characterized by emboldened\n autocrats, beleaguered democracies, and the United States’ withdrawal from its leadership\n role in the global struggle for human freedom.",
             x = 0.65, y = 0.24, fontfamily = "Arial", size = 6.5)

# Save figure
ggsave("2022/w8/political-freedom-around-the-world.png", width = 8, height = 6)
