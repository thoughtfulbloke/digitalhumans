library(atus)
library(dplyr)
library(ggplot2)
library(ggthemes)
data(atusact)
data(atusresp)
slide <- atusresp %>% filter(tuyear==2016) %>% 
  left_join(atusact %>% filter(tiercode >= 180000, tiercode < 190000), by="tucaseid") %>%
  count(dur) %>%
  ggplot(aes(x=dur, y=n, xend=dur)) + ylab("\nCount of actions\n") +
  ggtitle("American time use survey, 2016:
minutes spent journeying (travelling for a purpose)") + 
  xlab("\nNumber of minutes of survey day\n") +
  labs(caption = "source: BLS via R package ATUS\n") +
  geom_segment(yend=0, size=0.5,colour=pal_otago[4]) + theme_otago_ppt
#print(slide)

