# data manipulation, require pct10 and pct10

pct10_var <- pct10 %>% 
  mutate(min = value) %>%
  select(X, Subject, TimeWork, min, side, Muscle, Equipment)

pct90_var <- pct90 %>% 
  mutate(max = value) %>%
  select(max)

data_var <- cbind(pct10_var, pct90_var)
data_var <- data_var %>% mutate(range = max - min)

ggplot(data_var, aes(x = Muscle, y = range)) + 
  # coord_cartesian(ylim = c(0, 2000)) +
  geom_boxplot(aes(fill = Equipment, color = Muscle)) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_manual(values=c("black", "red")) +
  ylab("% RVC") +
  ggtitle("Muscle Activity Normalized to Reference Voluntary Contraction") +
  theme(plot.title = element_text(face = "bold")) #+
  #facet_grid(. ~ Percentile)

shapiro.test(log(data_var$range))

summary(gam(log(range) ~ Equipment + Muscle + TimeWork,#+ Equipment*Muscle + Equipment*TimeWork + Muscle*TimeWork,
            random = ~Equipment|Subject, 
            data = data_var, family = "scat"))

summary(gam(log(range) ~ Equipment + Muscle + TimeWork + Equipment*Muscle + Equipment*TimeWork + Muscle*TimeWork,
            random = ~Equipment|Subject, 
            data = data_var, family = "scat"))

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
