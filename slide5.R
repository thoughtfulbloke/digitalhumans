library(foreign) # read NZESin SPSS files
library(dplyr) # general data manipulation
library(tidyr) # restructuring
library(ggplot2)
library(ggthemes)

# NZES files in nzes companion folder
nz17 <- suppressWarnings(read.spss("../elect/nzes/NZES2017Release14-07-19.sav",
                                   to.data.frame = TRUE, add.undeclared.levels = "sort"))

nz02 <- suppressWarnings(read.spss("../elect/nzes/nzes_02_1.1.por",
                                   to.data.frame = TRUE, add.undeclared.levels = "sort"))


w17 <- nz17 %>% filter(!is.na(rsamage), !is.na(rtreaty), rtreaty != "9. Don't know") %>% 
  group_by(flage=floor(rsamage)) %>% 
  summarise(sa17 = sum((rtreaty == "1. Strongly agree") *rwt), 
            ag17 = sum((rtreaty == "2. Somewhat agree") *rwt),
            ne17 = sum((rtreaty == "3. Neither") *rwt), 
            ds17 = sum((rtreaty == "4. Somewhat disagree") *rwt),
            sd17 = sum((rtreaty == "5. Strongly disagree") *rwt)) %>%
  ungroup() %>% mutate(age2002=flage, age2017=flage)

w02 <- nz02 %>% filter(!is.na(WNAGE), !is.na(WTREAT), !is.na(ALZWT)) %>% 
  group_by(flage=floor(WNAGE)) %>% 
  summarise(sa02 = sum((WTREAT == "Strongly Agree") *ALZWT), 
            ag02 = sum((WTREAT == "Agree") *ALZWT),
            ne02 = sum((WTREAT == "Neutral") *ALZWT), 
            ds02 = sum((WTREAT == "Disagree") *ALZWT),
            sd02 = sum((WTREAT == "Strongly Disagree") *ALZWT)) %>%
  ungroup() %>% mutate(age2017=flage+15)

merged_years <- w17 %>% left_join(w02, by="age2017") %>%
  mutate(age_in_02 = case_when(age2017 < 33 ~ "03-17",
                               age2017 < 43 ~ "18-27",
                               age2017 < 53 ~ "28-37",
                               age2017 < 63 ~ "38-47",
                               age2017 < 73 ~ "48-57",
                               age2002 >= 58 ~ "58-67")) %>%
  group_by(age_in_02) %>% 
  summarise(old_sa = sum(sa02), old_ag = sum(ag02), old_ne=sum(ne02),  old_ds = sum(ds02), old_sd = sum(sd02),
            new_sa = sum(sa17), new_ag = sum(ag17), new_ne=sum(ne17),  new_ds = sum(ds17), new_sd = sum(sd17),
            old_sa_p = 100*old_sa/(old_sa + old_ag + old_ne + old_ds + old_sd),
            old_ag_p = 100*old_ag/(old_sa + old_ag + old_ne + old_ds + old_sd),
            old_ne_p = 100*old_ne/(old_sa + old_ag + old_ne + old_ds + old_sd),
            old_ds_p = 100*old_ds/(old_sa + old_ag + old_ne + old_ds + old_sd),
            old_sd_p = 100*old_sd/(old_sa + old_ag + old_ne + old_ds + old_sd),
            new_sa_p = 100*new_sa/(new_sa + new_ag + new_ne + new_ds + new_sd),
            new_ag_p = 100*new_ag/(new_sa + new_ag + new_ne + new_ds + new_sd),
            new_ne_p = 100*new_ne/(new_sa + new_ag + new_ne + new_ds + new_sd),
            new_ds_p = 100*new_ds/(new_sa + new_ag + new_ne + new_ds + new_sd),
            new_sd_p = 100*new_sd/(new_sa + new_ag + new_ne + new_ds + new_sd)) %>%
  ungroup() %>% select(age_in_02, old_sa_p:new_sd_p) %>% gather(category, percent, old_sa_p:new_sd_p) %>%
  separate(category, into=c("Time","Opine","permark"), sep="_") %>%
  mutate(election = ifelse(Time=="old", "2002", "2017"),
         Opinion = case_when(Opine == "sa" ~ "Strongly\nAgree",
                             Opine == "ag" ~ "Agree",
                             Opine == "ne" ~ "Neither",
                             Opine == "ds" ~ "Disagree",
                             TRUE ~ "Strongly\nDisagree"),
         Opinion = factor(Opinion, levels=c("Strongly\nAgree","Agree","Neither","Disagree","Strongly\nDisagree")))

slide <- ggplot(merged_years, aes(x=election,y=percent,colour=age_in_02, group=age_in_02, shape=age_in_02, fill=age_in_02)) + 
  geom_line(size=1) + facet_wrap(~Opinion, ncol=5) + geom_point(size=2.8) +
  scale_colour_viridis_d(option = "plasma", end=0.93, name="Age in\n2002") +
  ggtitle("15 Years of change:\nNZES responses to 'Do you agree with removing the Treaty of Waitangi
from the law', by age cohort") + xlab("\nElection study year\n") +
  ylab("\nPercent of cohort\n") + theme_otago_facet_ppt +
  scale_shape_manual(values=c(21,22,23,24,25,0), name="Age in\n2002") + 
  scale_fill_viridis_d(option = "plasma", end=0.93, name="Age in\n2002") +
  labs(caption = "source: New Zealand Election Study 2002, 2017. Datasets: http://www.nzes.org\n") 
#print(slide)
