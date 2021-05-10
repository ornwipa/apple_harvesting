setwd("~/Documents/Dissertation/WIP EMG")

library(tidyverse)
data <- readr::read_csv("Subj703_1sec_R_DM.csv")
tidy_data <- data %>% spread(Parameter, Value)
tidy_data <- tidy_data %>% select(-"<NA>")
tidy_data <- slice(tidy_data, 1:(n()-1))
