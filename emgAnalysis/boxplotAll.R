setwd("~/Documents/Dissertation/WIP EMG")

library(tidyverse)
data <- readr::read_csv("emg_normalized_nobreak.csv")
tidy_data <- data %>% gather(parameter, value, `pct10.0.ref`:`pct90.1.dyn`)
# tidy_data <- tidy_data %>% mutate(paramsplit = str_split(parameter, pattern = "[.]"))
tidy_data <- tidy_data %>% mutate(Percentile = substr(parameter, 1, 5),
                                  side = substr(parameter, 7, 7),
                                  reference = substr(parameter, 9, 11)) %>%
  mutate(Muscle = ifelse(side == 1, "Dominant", "Non-dominant"),
         Equipment = ifelse(Equipment == "Ground", "2_Ground", Equipment),
         Equipment = ifelse(Equipment == "Platform", "1_Platform", Equipment),
         Equipment = ifelse(Equipment == "Ladder", "0_Ladder", Equipment),
         value = value*100,
         Percentile = ifelse(Percentile == "pct10", "10th", 
                             ifelse(Percentile == "pct50", "50th", "90th")))
emg_ref <- tidy_data %>% filter(reference == "ref")

library(ggplot2)
ggplot(emg_ref, aes(x = Percentile, y = value)) + 
  coord_cartesian(ylim = c(0, 2000)) +
  geom_boxplot(aes(fill = Equipment, color = Muscle)) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_brewer(palette = "Dark2") + 
  ylab("% RVC") +
  ggtitle("Muscle Activity Normalized to Reference Voluntary Contraction") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"))

ggplot(emg_ref, aes(x = Muscle, y = value)) + 
  # coord_cartesian(ylim = c(0, 2000)) +
  geom_boxplot(aes(fill = Equipment, color = Muscle)) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_manual(values=c("black", "red")) +
  ylab("% RVC") +
  ggtitle("Muscle Activity Normalized to Reference Voluntary Contraction") +
  theme(plot.title = element_text(face = "bold")) +
  facet_grid(. ~ Percentile)
