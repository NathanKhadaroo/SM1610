# Loading libraries -------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(janitor)
library (patchork)

# loading data ------------------------------------------------------------

fms_clean <- read_csv("./Data/clean_fms.csv")

# Exploring data ----------------------------------------------------------

fms_clean

#counting NA's

fms_clean %>% select_if(function(x) any(is.na(x))) %>% 
  map(~sum(is.na(.)))
 
#looking for most common categories

fms_clean %>% 
  count(category, sort = TRUE)


# fms_clean %>% 
#   filter(year %in% 2012 :2018) %>% 
#   select(year, category)
# 
# fms_filter <- fms_clean %>%
#   filter(category %in% c( "Pavements/footpaths",
#                       "Dog Fouling",
#                       "Street lighting "),
#          year %in% 2012 :2018,
#          day %in% 1:31) %>% 
#    transmute(date = paste0(year, "-", month, "-", day ),
#            date = lubridate::ymd(date),
#            category) %>% 
#   group_by(category, date) %>% 
#   tally() %>% 
#   pivot_wider(names_from = category,
#               values_from = n) %>% 
#   clean_names()

#seeing how 

fms_clean %>%
  filter(category %in% c( "Pavements/footpaths",
                          "Dog Fouling",
                          "Street lighting "),
         year %in% 2012 :2018,
         day %in% 1:31) %>% 
  transmute(date = paste0(year, "-", month, "-", day ),
            date = lubridate::ymd(date),
            category) %>% 
  group_by(category, date) %>% 
  tally() %>% 
  ggplot(aes(date,
                          n,
                          colour = category))+
  geom_point()+
  geom_smooth()





?subrewing_filtered %>%ggplot(aes(date, 
                               category,
                               colour = category))+
  geom_point()
 



