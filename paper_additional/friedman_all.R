# non-parametric complete block design
# change `param` to `pct10`, `pct50`, `pct90`

lddr <- pct90 %>% filter(Equipment == "0_Ladder") %>% 
  filter(Subject == "Subj713" | Subject == "Subj714" |
           Subject == "Subj715" | Subject == "Subj716") %>%
  # note: omit Subj709 and Subj711 as they have only one side
  filter(TimeWork <= 360 & Muscle == "Non-dominant")
friedman.test(y=lddr$value, groups=factor(lddr$TimeWork), blocks=lddr$Subject) 

lddr <- pct90 %>% filter(Equipment == "0_Ladder") %>% 
  filter(Subject == "Subj713" | Subject == "Subj714" |
           Subject == "Subj715" | Subject == "Subj716") %>%
  # note: omit Subj709 and Subj711 as they have only one side
  filter(TimeWork <= 360 & Muscle == "Dominant")
friedman.test(y=lddr$value, groups=factor(lddr$TimeWork), blocks=lddr$Subject)

pltf <- pct90 %>% filter(Equipment == "1_Platform") %>% 
  filter(Subject == "Subj701" | Subject == "Subj702" |
           Subject == "Subj717" | Subject == "Subj718" |
           Subject == "Subj719" | Subject == "Subj720") %>%
  filter(TimeWork <= 360 & Muscle == "Non-dominant")
friedman.test(y=pltf$value, groups=factor(pltf$TimeWork), blocks=pltf$Subject) 

pltf <- pct90 %>% filter(Equipment == "1_Platform") %>% 
  filter(Subject == "Subj701" | Subject == "Subj702" |
           Subject == "Subj717" | Subject == "Subj718" |
           Subject == "Subj719" | Subject == "Subj720") %>%
  filter(TimeWork <= 360 & Muscle == "Dominant")
friedman.test(y=pltf$value, groups=factor(pltf$TimeWork), blocks=pltf$Subject) 

grnd <- pct90 %>% filter(Equipment == "2_Ground") %>% 
  filter(Subject == "Subj705" | Subject == "Subj706" |
           Subject == "Subj708" | Subject == "Subj721" |
           Subject == "Subj722" | Subject == "Subj724") %>%
  filter(TimeWork <= 180 & Muscle == "Non-dominant")
friedman.test(y=grnd$value, groups=factor(grnd$TimeWork), blocks=grnd$Subject) 

grnd <- pct90 %>% filter(Equipment == "2_Ground") %>% 
  filter(Subject == "Subj705" | Subject == "Subj706" |
           Subject == "Subj708" | Subject == "Subj721" |
           Subject == "Subj722" | Subject == "Subj724") %>%
  filter(TimeWork <= 180 & Muscle == "Dominant")
friedman.test(y=grnd$value, groups=factor(grnd$TimeWork), blocks=grnd$Subject) 
