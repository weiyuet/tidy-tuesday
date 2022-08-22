# Loading libraries
library(tidyverse)
library(tidytuesdayR)
library(scales)

# Loading data
tt <- tt_load("2021-04-06")

# Wrangling data
# Creating a function to tidy the tt$brazil_loss data set
## The function "tidy_brazil_loss" takes a variable name as an argument
## It returns that variable in a tidy format, with one row per observation and one column per variable
tidy_brazil_loss <- function(variable){
  tt$brazil_loss %>% 
    select(year, variable) %>% 
    mutate(cause = as.character(variable)) %>% 
    select(year, cause, variable) %>% 
    rename("loss" = variable)}

# Creating a list of variables that will be run through the tidy_brazil_loss function
## First three variables skipped (entity, code, year): entity and code irrelevant because all observations from Brazil
## Year is included in each observation in tidy data set
variable_names <- colnames(tt$brazil_loss)[4:14]
# Applying the tidy_brazil_loss function to all variable names using purr::map
tidy_brazil_list <- map(variable_names, tidy_brazil_loss)
# Binding tidy version of each variable by row using purr::map_df
tidy_brazil <- map_df(tidy_brazil_list, rbind)

# Changing the "cause" variable in tidy_brazil to a factor variable
tidy_brazil$cause <- as.factor(tidy_brazil$cause)
# Print levels of tidy_brazil$cause
levels(tidy_brazil$cause)

# Editing tidy_brazil$cause factor levels for better descriptions
levels(tidy_brazil$cause) <- c("Commercial crops",
                               "Fire loss",
                               "Flooding due to dams",
                               "Mining",
                               "Natural disturbances",
                               "Infrastructure (not roads)",
                               "Pasture for livestock",
                               "Roads",
                               "Logging for lumber",
                               "Small scale clearing",
                               "Tree plantations")

# Print tidied version of tt$brazil_loss
tidy_brazil

# Plotting the causes of deforestation in Brazil
tidy_brazil %>% 
  ggplot(aes(year, loss, colour = cause)) +
  geom_line(colour = "gray35" ,size = 0.8) +
  geom_hline(yintercept = 10000, linetype = 2, size = 0.5) +
  facet_wrap(~cause, scales = "free") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  theme_classic() + 
  guides(colour = "none") +
  labs(x = "",
       y = "Forest loss (hectares)",
       title = "Deforestation in Brazil",
       subtitle = "Dashed line for perspective (10,000 hectares is about 10,000 rugby fields)",
       caption = "Source: Our World in Data")

ggsave("2021/w15/deforestation-in-brazil.png", width = 8, height = 6)