grf <- v_for_visitor %>%
  count(user_id) %>%
  inner_join(r_for_radicalism, by=c("user_id" = "xliker")) %>%
  inner_join(c_for_country, by=c("user_id" = "xid")) %>% 
  rename(place = locat) %>%
  filter(place %in% c("NZ","AU","GB","US")) %>%
  mutate(
    balance = as.numeric(nett_fa) - 
      (as.numeric(nett_nz) + as.numeric(nett_au) + 
         as.numeric(nett_gb) + as.numeric(nett_us)),
    focus = (as.numeric(nett_fa) + as.numeric(nett_nz) + as.numeric(nett_au) + 
               as.numeric(nett_gb) + as.numeric(nett_us))/as.numeric(allike)
  ) %>%
  count(user_id, place, balance,focus, n) 
slide <- ggplot(grf, aes(x = balance, y=log2(focus + 0.01), colour=place)) +
  geom_vline(xintercept = 0, alpha=0.4) + geom_jitter(alpha=0.3) +
  ggtitle("Polarisation Irises: Involvement(Vertical) vs Sympathies(Horizontal)") +
  facet_wrap(~ place, nrow=2) + scale_colour_manual(values=pal_otago[c(2,4,3,1)]) +
  scale_y_continuous(NULL, breaks = NULL) + 
  scale_x_continuous(NULL, breaks = NULL) +
  theme(plot.title = element_text(colour = "#00508F", margin=margin(t=5, b=5, unit = "pt"),
                                  family = "Open Sans SemiBold",lineheight=1.12,
                                  hjust=0.5),
        legend.position="none", strip.placement = "inside",
        plot.background = element_rect(fill = "#FAFAFA", colour = NA),
        strip.background = element_rect(fill= "#FFFFFF", colour="#EFEFEF"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#EFEFEF"),
        strip.text = element_text(size=13, margin=margin(t=3, b=3, unit = "pt"),
                                  family = "Open Sans Regular")) 

