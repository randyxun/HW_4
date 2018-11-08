library(RCurl)
library(dplyr)
library(tidyverse)
library(tibble)
library(purrr)
library(broom)
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
baltimore <- filter(unsolved, city_name == "Baltimore, MD" )
proportion_baltimore <- prop.test(x = baltimore$total_unsolved, 
                                  n = baltimore$total_homicides)
tidy_baltimore <- proportion_baltimore %>% 
  tidy() %>% 
  select(estimate, conf.low, conf.high)

proportion_unsolved <- unsolved %>% 
  mutate(test = map2(total_unsolved, total_homicides,
                     ~ prop.test(.x, n = .y)))

