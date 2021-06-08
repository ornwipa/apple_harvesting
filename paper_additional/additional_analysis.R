# data manipulation, require pct10 and pct10

library(tidyverse)

data_var <- cbind(
  pct10 %>% mutate(min = value) %>% select(Subject, TimeWork, min, Muscle, Equipment), 
  pct90 %>% mutate(max = value) %>% select(max))
data_var <- data_var %>% mutate(range = max - min)

# data visualization: boxplot without time effect

library(ggplot2)

ggplot(data_var, aes(x = Muscle, y = range)) + 
  geom_boxplot(aes(fill = Equipment, color = Muscle)) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_manual(values=c("black", "red")) +
  ylab("% RVC") +
  ggtitle("Muscle Activity Normalized to Reference Voluntary Contraction") +
  theme(plot.title = element_text(face = "bold"))

# statistical analysis: non-parametric
shapiro.test(log(data_var$range)) # not normally distributed

library(mgcv)  

summary(gam(log(range) ~ Equipment + Muscle + TimeWork,
            random = ~Equipment|Subject, 
            data = data_var, family = "scat"))

summary(gam(log(range) ~ Equipment + Muscle + TimeWork + Equipment*Muscle + Equipment*TimeWork + Muscle*TimeWork,
            random = ~Equipment|Subject, 
            data = data_var, family = "scat"))

# data visualization: changes over time

ggplot(data_var, aes(x = TimeWork, y = range, color = Equipment)) + 
  stat_smooth() +
  xlim(c(0,220)) +
  geom_vline(xintercept = 150, linetype="dotted") +
  scale_color_brewer(palette = "Dark2") + 
  xlab("Work Time (minutes)") +
  ylab("% RVC") +
  ggtitle("Muscle Activity Changes over Time") +
  theme(plot.title = element_text(face = "bold")) +
  facet_grid(. ~ Muscle, scales = "free_y")

# data manipulation: split/group by equipment

data_var %>% group_by(Equipment) %>% summarise(n = n())

lddr <- data_var %>% filter(Equipment == "0_Ladder") %>% 
  filter(Subject == "Subj713" | Subject == "Subj714" |
           Subject == "Subj715" | Subject == "Subj716") %>%
  # note: omit Subj709 and Subj711 as they have only one side
  filter(TimeWork <= 360 & Muscle == "Non-dominant")
friedman.test(y=lddr$range, groups=factor(lddr$TimeWork), blocks=lddr$Subject) 

lddr <- data_var %>% filter(Equipment == "0_Ladder") %>% 
  filter(Subject == "Subj713" | Subject == "Subj714" |
           Subject == "Subj715" | Subject == "Subj716") %>%
  # note: omit Subj709 and Subj711 as they have only one side
  filter(TimeWork <= 360 & Muscle == "Dominant")
friedman.test(y=lddr$range, groups=factor(lddr$TimeWork), blocks=lddr$Subject) 

pltf <- data_var %>% filter(Equipment == "1_Platform") %>% 
  filter(Subject == "Subj701" | Subject == "Subj702" |
           Subject == "Subj717" | Subject == "Subj718" |
           Subject == "Subj719" | Subject == "Subj720") %>%
  filter(TimeWork <= 360 & Muscle == "Non-dominant")
friedman.test(y=pltf$range, groups=factor(pltf$TimeWork), blocks=pltf$Subject) 

pltf <- data_var %>% filter(Equipment == "1_Platform") %>% 
  filter(Subject == "Subj701" | Subject == "Subj702" |
           Subject == "Subj717" | Subject == "Subj718" |
           Subject == "Subj719" | Subject == "Subj720") %>%
  filter(TimeWork <= 360 & Muscle == "Dominant")
friedman.test(y=pltf$range, groups=factor(pltf$TimeWork), blocks=pltf$Subject) 

grnd <- data_var %>% filter(Equipment == "2_Ground") %>% 
  filter(Subject == "Subj705" | Subject == "Subj706" |
           Subject == "Subj708" | Subject == "Subj721" |
           Subject == "Subj722" | Subject == "Subj724") %>%
  filter(TimeWork <= 180 & Muscle == "Non-dominant")
friedman.test(y=grnd$range, groups=factor(grnd$TimeWork), blocks=grnd$Subject) 

grnd <- data_var %>% filter(Equipment == "2_Ground") %>% 
  filter(Subject == "Subj705" | Subject == "Subj706" |
           Subject == "Subj708" | Subject == "Subj721" |
           Subject == "Subj722" | Subject == "Subj724") %>%
  filter(TimeWork <= 180 & Muscle == "Dominant")
friedman.test(y=grnd$range, groups=factor(grnd$TimeWork), blocks=grnd$Subject) 
