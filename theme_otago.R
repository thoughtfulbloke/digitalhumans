library(ggplot2)
library(ggthemes)
# pallette of four Otago colours plus four complementary if needed
pal_otago <- c("#00508F", "#f9c013", "#1b1c20", "#ce2227", "#8f5800", "#22cec5", "#f9f913", "#8613f9")

# theme_otago_o uses Open Sans and Otago blue
# so needs the font Open Sans installed and available

theme_otago <- theme_minimal() + 
  theme(plot.title = element_text(colour = "#00508F", lineheight=1.08,
                                  family = "Open Sans Light"),
        panel.grid = element_blank(),
        axis.line.x = element_line(size=0.1,
                                   colour = "#00508F"),
        axis.line.y = element_line(size=0.1,
                                   colour = "#00508F"),
        axis.text = element_text(colour = "#00508F",
                                 family = "Open Sans"),
        axis.title = element_text(colour = "#00508F",
                                 family = "Open Sans Light"),#trbl
        axis.ticks = element_line(size=0.2,
                                  colour = "#00508F"),
        legend.text = element_text(colour = "#00508F",
                                   family = "Open Sans Light"),
        legend.title = element_text(colour = "#00508F",
                                    family = "Open Sans")
          )

theme_otago_ppt <- theme_minimal() + 
  theme(plot.title = element_text(colour = pal_otago[1], margin=margin(b = 12, unit = "pt"),
                                  family = "Open Sans Bold", lineheight=1.16),
        panel.grid = element_blank(),
        axis.line.x = element_line(size=0.1,
                                   colour = pal_otago[1]),
        axis.line.y = element_line(size=0.1,
                                   colour = pal_otago[1]),
        axis.text = element_text(colour = pal_otago[1], size = 12,
                                 family = "Open Sans SemiBold"),
        axis.title = element_text(colour = pal_otago[1], size = 13,
                                  lineheight=1.08, hjust=0,
                                  family = "Open Sans SemiBold"),#trbl
        axis.ticks = element_line(size=0.2,
                                  colour = pal_otago[1]),
        legend.text = element_text(colour = pal_otago[1], size=12,
                                   family = "Open Sans"),
        legend.title = element_text(colour = pal_otago[1], lineheight=1.08, hjust=1,
                                    family = "Open Sans SemiBold", size=13),
        plot.caption = element_text(size=12, family = "Open Sans Regular",
                                    hjust=1)
  )

theme_otago_facet_ppt <- theme_minimal() + 
  theme(plot.title = element_text(colour = pal_otago[1], margin=margin(b = 12, unit = "pt"),
                                  family = "Open Sans Bold", lineheight=1.16),
        panel.grid = element_blank(),
        axis.line.x = element_line(size=0.1,
                                   colour = pal_otago[1]),
        axis.line.y = element_line(size=0.1,
                                   colour = pal_otago[1]),
        axis.text = element_text(colour = pal_otago[1], size = 12,
                                 family = "Open Sans SemiBold"),
        axis.title = element_text(colour = pal_otago[1], size = 13,
                                  lineheight=1.08, hjust=0,
                                  family = "Open Sans SemiBold"),#trbl
        axis.ticks = element_line(size=0.2,
                                  colour = pal_otago[1]),
        legend.key.size = unit(2, 'lines'),
        legend.text = element_text(colour = pal_otago[1], size=12,
                                   family = "Open Sans"),
        legend.title = element_text(colour = pal_otago[1], lineheight=1.08, hjust=1,
                                    family = "Open Sans SemiBold", size=13),
        plot.caption = element_text(size=12, family = "Open Sans Regular",
                                    hjust=1),
        strip.background = element_rect(fill= "#FFFFFF", colour="#EFEFEF"),
        strip.placement = "inside", legend.position = "right",
        strip.text = element_text(size=13, margin=margin(t=3, b=3, unit = "pt"),
                                  family = "Open Sans Regular"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        panel.spacing = unit(1.5, "lines"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = NA)
  )


