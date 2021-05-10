# require emg_ref

library(tidyverse)

pct10 <- emg_ref %>% filter(Percentile == "10th")
pct50 <- emg_ref %>% filter(Percentile == "50th")
pct90 <- emg_ref %>% filter(Percentile == "90th")

#### Assumption checks ####

shapiro.test(log(pct10$value)) # p-value = 4.04e-11
shapiro.test(log(pct50$value)) # p-value = 4.152e-10
shapiro.test(log(pct90$value)) # p-value = 1.119e-09

library(ggpubr)

ggqqplot(pct10$value)
ggqqplot(pct50$value)
ggqqplot(pct90$value)
ggqqplot(log(pct10$value))
ggqqplot(log(pct50$value))
ggqqplot(log(pct90$value))

# log-transformed data did not pass normality test; varainces are also clearly not uniform.

#### Non-parametric tests ####

# use paired samples Wilcoxon signed-rank test for differences between two sides.

pct10dm <- pct10 %>% filter(side == "1")
pct10nd <- pct10 %>% filter(side == "0")
wilcox.test(pct10dm$value, pct10nd$value, paired = TRUE, alternative = "two.sided")
# p-value = 0.0291

pct50dm <- pct50 %>% filter(side == "1")
pct50nd <- pct50 %>% filter(side == "0")
wilcox.test(pct50dm$value, pct50nd$value, paired = TRUE, alternative = "two.sided")
# p-value = 0.06468

pct90dm <- pct90 %>% filter(side == "1")
pct90nd <- pct90 %>% filter(side == "0")
wilcox.test(pct90dm$value, pct90nd$value, paired = TRUE, alternative = "two.sided")
# p-value = 5.157e-08

# separate two muscle sides to test for equipment effect on muscle activity.
# use non-parametric Kruskal-Wallis test for one-way ANOVA

kruskal.test(value ~ Equipment, data = pct10dm) # p-value = 0.0005998
kruskal.test(value ~ Equipment, data = pct10nd) # p-value = 0.01659
kruskal.test(value ~ Equipment, data = pct50dm) # p-value = 0.0008818
kruskal.test(value ~ Equipment, data = pct50nd) # p-value = 0.03859
kruskal.test(value ~ Equipment, data = pct90dm) # p-value = 0.001253
kruskal.test(value ~ Equipment, data = pct90nd) # p-value = 0.08544

# average muscle activity through time before statistical test

pct10dm_prep <- pct10dm %>% 
  group_by(Subject, Equipment) %>% 
  summarise(Value = mean(na.omit(value)))
kruskal.test(Value ~ Equipment, data = pct10dm_prep) # p-value = 0.6286
pct10nd_prep <- pct10nd %>% 
  group_by(Subject, Equipment) %>% 
  summarise(Value = mean(na.omit(value)))
kruskal.test(Value ~ Equipment, data = pct10nd_prep) # p-value = 0.8785
pct50dm_prep <- pct50dm %>% 
  group_by(Subject, Equipment) %>% 
  summarise(Value = mean(na.omit(value)))
kruskal.test(Value ~ Equipment, data = pct50dm_prep) # p-value = 0.6683
pct50nd_prep <- pct50nd %>% 
  group_by(Subject, Equipment) %>% 
  summarise(Value = mean(na.omit(value)))
kruskal.test(Value ~ Equipment, data = pct50nd_prep) # p-value = 0.9013
pct90dm_prep <- pct90dm %>% 
  group_by(Subject, Equipment) %>% 
  summarise(Value = mean(na.omit(value)))
kruskal.test(Value ~ Equipment, data = pct90dm_prep) # p-value = 0.6427
pct90nd_prep <- pct90nd %>% 
  group_by(Subject, Equipment) %>% 
  summarise(Value = mean(na.omit(value)))
kruskal.test(Value ~ Equipment, data = pct90nd_prep) # p-value = 0.9013

#### Generalized Additive Mixed Models ####

# treat equipment as fixed effect, subject nested in equipment as random effect.

library(mgcv)  

summary(gam(log(value) ~ Equipment + Muscle + Equipment*Muscle, 
            random = ~Equipment|Subject, 
            data = pct10, family = "scat"))
# Family: Scaled t(50.978,0.852) 
# Link function: identity 
# 
# Formula:
#   log(value) ~ Equipment + Muscle + Equipment * Muscle
# 
# Parametric coefficients:
#                                                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                       4.661349   0.064042  72.786  < 2e-16 ***
# EquipmentPlatform_elevated                       -0.215916   0.083297  -2.592  0.00954 ** 
# EquipmentPlatform_ground                         -0.289386   0.097624  -2.964  0.00303 ** 
# MuscleRight(dominant)                             0.143448   0.093747   1.530  0.12598    
# EquipmentPlatform_elevated:MuscleRight(dominant) -0.029370   0.119206  -0.246  0.80538    
# EquipmentPlatform_ground:MuscleRight(dominant)   -0.008619   0.140167  -0.061  0.95097    
# 
# R-sq.(adj) =  0.016   Deviance explained = 2.14%
# -REML = 1518.6  Scale est. = 1         n = 1181

summary(gam(log(value) ~ Equipment + Muscle + Equipment*Muscle, 
            random = ~Equipment|Subject, 
            data = pct50, family = "scat"))
# Family: Scaled t(48.147,0.85) 
# Link function: identity 
# 
# Formula:
#   log(value) ~ Equipment + Muscle + Equipment * Muscle
# 
# Parametric coefficients:
#                                                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                       5.21418    0.06392  81.568  < 2e-16 ***
# EquipmentPlatform_elevated                       -0.20715    0.08314  -2.491  0.01272 *  
# EquipmentPlatform_ground                         -0.26598    0.09745  -2.730  0.00634 ** 
# MuscleRight(dominant)                             0.09515    0.09358   1.017  0.30926    
# EquipmentPlatform_elevated:MuscleRight(dominant) -0.05283    0.11899  -0.444  0.65705    
# EquipmentPlatform_ground:MuscleRight(dominant)   -0.05544    0.13991  -0.396  0.69191    
# 
# R-sq.(adj) =  0.0124   Deviance explained = 1.79%
# -REML = 1516.5  Scale est. = 1         n = 1181

summary(gam(log(value) ~ Equipment + Muscle + Equipment*Muscle, 
            random = ~Equipment|Subject, 
            data = pct90, family = "scat"))
# Family: Scaled t(47.187,0.854) 
# Link function: identity 
# 
# Formula:
#   log(value) ~ Equipment + Muscle + Equipment * Muscle
# 
# Parametric coefficients:
#                                                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                       5.73539    0.06426  89.259   <2e-16 ***
# EquipmentPlatform_elevated                       -0.19315    0.08357  -2.311   0.0208 *  
# EquipmentPlatform_ground                         -0.23722    0.09795  -2.422   0.0154 *  
# MuscleRight(dominant)                             0.06983    0.09406   0.742   0.4578    
# EquipmentPlatform_elevated:MuscleRight(dominant) -0.09469    0.11960  -0.792   0.4285    
# EquipmentPlatform_ground:MuscleRight(dominant)   -0.09918    0.14063  -0.705   0.4806    
# 
# R-sq.(adj) =  0.0118   Deviance explained = 1.72%
# -REML = 1522.6  Scale est. = 1         n = 1181

# Reference
# https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.html
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/family.html
# scaled t for heavy tailed data that would otherwise be modelled as Gaussian