library(RCurl)
library(dplyr)
library(tidyverse)
library(tibble)
library(purrr)
library(broom)
library(scales)

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
                     ~ prop.test(.x, n = .y))) %>% 
  mutate(test = map(test,
                    ~ tidy(.x))) %>% 
  unnest(.drop = TRUE) %>% 
  select(city_name, estimate, conf.low, conf.high)

proportion_unsolved %>% 
  mutate(city_name = fct_reorder(city_name, estimate)) %>% 
  filter(city_name != "Tulsa, AL") %>% 
  ggplot(aes(x = estimate, y = city_name)) +
  geom_point(color= "white") +
  theme_dark() +
  labs(title = "Unsolved homicides by city", 
       subtitle = " Bars show 95% confidence interval", 
       x = NULL, y = "Percent of homicides that are unsolved") +
  scale_x_continuous(limits = c(0.2, 0.75),
                     breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                     labels = percent) +
  geom_errorbarh(color = "white", 
                aes(y = city_name, xmin = conf.low,
                    xmax = conf.high, height = 0)) 

