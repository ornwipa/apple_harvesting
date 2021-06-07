library(mgcv)  

summary(gam(log(value) ~ Equipment + Muscle + TimeWork,#+ Equipment*Muscle + Equipment*TimeWork + Muscle*TimeWork,
            random = ~Equipment|Subject, 
            data = pct10, family = "scat"))

summary(gam(log(value) ~ Equipment + Muscle + TimeWork,#+ Equipment*Muscle + Equipment*TimeWork + Muscle*TimeWork,
            random = ~Equipment|Subject, 
            data = pct50, family = "scat"))

summary(gam(log(value) ~ Equipment + Muscle + TimeWork,#+ Equipment*Muscle + Equipment*TimeWork + Muscle*TimeWork,
            random = ~Equipment|Subject, 
            data = pct90, family = "scat"))

summary(gam(log(value) ~ Equipment + TimeWork + Equipment*TimeWork, 
            random = ~Equipment|Subject, 
            data = pct10, family = "scat"))

summary(gam(log(value) ~ Equipment + TimeWork + Equipment*TimeWork, 
            random = ~Equipment|Subject, 
            data = pct50, family = "scat"))

summary(gam(log(value) ~ Equipment + TimeWork + Equipment*TimeWork, 
            random = ~Equipment|Subject, 
            data = pct90, family = "scat"))

library(ggplot2)

ggplot(emg_ref, aes(x = TimeWork, y = value, color = Equipment)) + 
  stat_smooth() +
  xlim(c(0,220)) +
  geom_vline(xintercept = 150, linetype="dotted") +
  scale_color_brewer(palette = "Dark2") + 
  xlab("Work Time (minutes)") +
  ylab("% RVC") +
  ggtitle("Muscle Activity Changes over Time") +
  theme(plot.title = element_text(face = "bold")) +
  facet_grid(Percentile ~ Muscle, scales = "free_y")
