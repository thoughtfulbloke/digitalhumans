library(atus)
library(dplyr)
library(ggplot2)
library(ggthemes)
data(atusact)
data(atusresp)
slide <- atusresp %>% filter(tuyear==2016) %>% 
  left_join(atusact %>% filter(tiercode == 120304), by="tucaseid") %>%
  mutate(dur = ifelse(is.na(dur), 0,dur)) %>%
  group_by(tucaseid) %>% summarise(Minutes = sum(dur)) %>%
  ungroup() %>% count(Minutes) %>%
  ggplot(aes(x=Minutes, xend=Minutes, y=n)) + ylab("\nCount of people\n") +
  ggtitle("American time use survey, 2016:
minutes respondents spent watching religious TV") + 
  xlab("\nNumber of minutes of survey day\n") +
  labs(caption = "source: BLS via R package ATUS\n") +
  geom_segment(yend=0, colour=pal_otago[4], size=2) + theme_otago_ppt 
#print(slide)


