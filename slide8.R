library(binom)

mark17 <- nz17 %>% filter(rethcomb1 == "1. European") %>%
  summarise(profas = sum((rimmiculture == "1. Strongly agree" & !is.na(rimmiculture) &
                            rtreaty == "1. Strongly agree" & !is.na(rtreaty))  * rwt),
            tot = sum(rwt),
            percent = round(100 * profas/tot, 2)) %>% mutate(Year=2017)


mark05 <- nz05 %>% filter((yetheur == "Yes") & !is.na(YPWT)) %>%
  summarise(profas = sum((yimmthr == "Strongly Agree" & !is.na(yimmthr) &
                            ytreaty == "Strongly Agree" & !is.na(ytreaty))  * YPWT),
            tot = sum(YPWT),
            percent = round(100 * profas/tot, 2)) %>% mutate(Year=2005)

porfas <- bind_rows(mark05,mark17)

binom_int <- function(x,df) {
  bc68 <- binom.confint(df$profas[x], df$tot[x], conf.level = 0.68268, methods = "wilson")
  bc95 <- binom.confint(df$profas[x], df$tot[x], conf.level = 0.95, methods = "wilson")
  return(data.frame(L68 = bc68$lower, U68 = bc68$upper,
                    L95 = bc95$lower, U95 = bc95$upper))
}

cinfs <- bind_rows(lapply(1:nrow(porfas), binom_int, df=porfas)) *100

slide <- porfas %>% bind_cols(cinfs) %>% select(Year, percent, L68:U95) %>% 
  gather(interv, yval, L68:U95) %>%
  separate(interv, into=c("LorU","conint"), sep=1, convert = T) %>%
  spread(LorU, yval) %>% arrange(Year, desc(conint)) %>%
  mutate(conint=factor(conint)) %>%
  ggplot(aes(x=Year,y=U,xmin=Year-.35, xmax=Year+.35, ymin=L,ymax=U,group=conint,fill=conint)) + 
  geom_rect() + scale_fill_manual(values=colorRampPalette(c(pal_otago[1],"white"))(3),
                                  name="Confidence\nInterval")+
  xlab("\nElection study year\n") + ylim(0,100) + ylab("\nPercent of respondents\n") +
  ggtitle("NZES NZ European/ European respondents who strongly agree both
'the Treaty of Waitangi should be removed from the law' &
'NZ culture is generally harmed by immigrants'") + 
  labs(caption = "source: New Zealand Election Study\n") +
  theme_otago_ppt

