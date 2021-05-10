# require tidy_data; return summary within 20-min sections

subsection1 <- tidy_data %>% slice(3000:4200) %>% sapply(mean)
subsection2 <- tidy_data %>% slice(4200:5000) %>% sapply(mean)
subsection3 <- tidy_data %>% slice(10800:12000) %>% sapply(mean)
subsection4 <- tidy_data %>% slice(13200:14400) %>% sapply(mean)

data.frame(rbind(subsection1, subsection2, subsection3, subsection4)) %>% 
  mutate(Subject = "Subj703", Side = "R", Utility = "DM") %>%
  write_csv(file = "Resampling_EMG.csv", append = TRUE)
  # write_csv(file = "Resampling_Subj703_1sec_R_DM.csv")
