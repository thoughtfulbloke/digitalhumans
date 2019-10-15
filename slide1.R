library(atus)
library(dplyr)
library(ggplot2)
library(ggthemes)
data(atusact)
data(atusresp)
slide <- atusresp %>% filter(tuyear==2016) %>% 
  inner_join(atusact %>% filter(tiercode == 120304), by="tucaseid") %>%
  ggplot(aes(x=dur))+ geom_dotplot(binwidth=1, dotsize=4, fill=pal_otago[4]) + 
  theme_otago_ppt +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("American time use survey, 2016:
minutes spent watching religious TV") + 
  xlab("\nNumber of minutes of survey day\n") +
  labs(caption = "source: BLS via R package ATUS\n")
#print(slide)
