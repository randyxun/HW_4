library(RCurl)
library(dplyr)
library(tidyverse)
library(tibble)
library(purrr)
homicides <- read.csv(text=getURL(
  "https://raw.githubusercontent.com/randyxun/HW_4/master/homicide-data.csv"))
  
homicides <- homicides %>% 
  unite(col = city_name, city, state, sep = ", ")

unsolved <- homicides %>% 
  select(city_name, disposition) %>% 
  group_by(city_name) %>% 
  mutate(unsolved = disposition != 'Closed by arrest') %>% 
  summarize(total_homicides = n(),
            total_unsolved = sum(unsolved))
