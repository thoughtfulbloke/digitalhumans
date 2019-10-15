library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(ggthemes)

set1 = "../fprog/canadian_racists"
set2 = "../fprog/mol_racist"
set3 = "../fprog/South_racist"
set4 = "../fprog/recent_direct"
set5 = "../fprog/recent_direct_regather600"
f1 <- paste0(set1, "/", list.files(set1, pattern=".csv$"))
f2 <- paste0(set2, "/", list.files(set2, pattern=".csv$"))
f3 <- paste0(set3, "/", list.files(set3, pattern=".csv$"))
f4 <- paste0(set4, "/", list.files(set4, pattern=".csv$"))
f5 <- paste0(set5, "/", list.files(set5, pattern=".csv$"))
full_list = c(f1,f2,f3,f4,f5)

v_for_visitor <- bind_rows(lapply(full_list, read_csv, col_types=cols(
  .default = col_character()))) %>% select(user_id, created_at) %>% distinct()
r_for_radicalism <- read.csv("../support_files/fa_spec.csv",
                             colClasses = "character") # were on spectrum account
c_for_country <- read.csv("../support_files/assignment.csv",
                          colClasses = "character")
c_for_country <- c_for_country %>% mutate(xid=paste0("x", user_id))

slide <- v_for_visitor %>% inner_join(c_for_country, by=c("user_id" = "xid")) %>%
  filter(locat %in% c("AU", "NZ", "US", "GB")) %>%
  mutate(hour_of_day = hour(ymd_hms(created_at))) %>% 
  count(locat, hour_of_day) %>% 
  group_by(locat) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=hour_of_day, y=prop, fill=locat)) + 
  ggtitle("\nDiscussion activity\nby geographic zone\n") +
  facet_wrap(~locat, nrow=2) +
  geom_col() + theme_void() + coord_polar() +
  scale_fill_manual(values=pal_otago[c(2,4,3,1)]) +
  theme(plot.title = element_text(colour = "#00508F", margin=margin(t=5, b=5, unit = "pt"),
                                  family = "Open Sans SemiBold",lineheight=1.12,
                                  hjust=0.5),
        legend.position="none", strip.placement = "inside",
        plot.background = element_rect(fill = "#FAFAFA", colour = NA),
        strip.background = element_rect(fill= "#FFFFFF", colour="#EFEFEF"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#EFEFEF"),
        strip.text = element_text(size=13, margin=margin(t=3, b=3, unit = "pt"),
                                  family = "Open Sans Regular")) 
