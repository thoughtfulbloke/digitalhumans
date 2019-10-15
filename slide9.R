library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

data_as_txt <- "
state, opposed, supportive
NZ, 25.5, 81.3
AU, 25.5, 0
US, 20.7, 12.5
UK, 10.9, 6.3
robots, 7.6, 0
anon, 8.2,0
other, 1.6, 0"

df <- read.csv(text = data_as_txt, stringsAsFactors = F)

slide <- df %>% gather(sentiment, percentage, opposed:supportive) %>%
  ggplot(aes(x=sentiment,y=percentage, fill=state)) +geom_col(width = 0.4) + coord_flip() +
  scale_fill_manual(values=pal_otago, name="Country")+ ylab("\nPercentage of responses\n") +
  ggtitle("Manually coded responses to Phil Goff by country and sentiment") + 
  xlab("\nResponse Sentiment\n") +
  labs(caption = "source: Twitter download via API\n") +
  theme_otago_ppt 
