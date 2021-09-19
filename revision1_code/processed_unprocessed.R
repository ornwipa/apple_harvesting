summary(tidy_data)
summary(data)

library(tidyverse)

data %>% filter(Equipment=="Ladder") %>% summary()
data %>% filter(Equipment=="Ground") %>% summary()
data %>% filter(Equipment=="Platform") %>% summary()

data %>% filter(TimeWork > 10) %>% filter(TimeWork <= 100) %>% filter(Equipment=="Ladder") %>% summary()
data %>% filter(TimeWork > 10) %>% filter(TimeWork <= 100) %>% filter(Equipment=="Ground") %>% summary()
data %>% filter(TimeWork > 10) %>% filter(TimeWork <= 100) %>% filter(Equipment=="Platform") %>% summary()