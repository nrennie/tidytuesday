library(tidyverse)
library(geofacet)
library(extrafont)
library(cowplot)

devtools::install_github("nrennie/usefunc")
library(usefunc)

tuesdata <- tidytuesdayR::tt_load('2021-10-05')
nurses <- tuesdata$nurses

colnames(nurses)[6] <- "Hourly 50th Percentile"
plot_data <- nurses[,c(1,2,6,10:13)] %>% 
  pivot_longer(cols=3:7, names_to="Percentile", values_to = "wage") %>%
  filter(State %notin% c("Guam", "Puerto Rico", "Virgin Islands")) %>%
  mutate(state_abb = US_name_to_abb(State)) 

p <- ggplot(plot_data, aes(x=Year, y=wage, group=Percentile, colour=Percentile)) +
  geom_line() +
  facet_geo(~ state_abb, grid = "us_state_grid2", label = "code") + 
  scale_x_continuous(breaks=c(2005,2020), labels=c("\'05","\'20")) +
  scale_colour_manual("Percentile", values=c("#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5"), 
                      labels=c("10th", "25th", "50th", "75th", "90th")) +
  labs(x="", y="$/hour", title="", 
       subtitle="", caption="N. Rennie | Data: Data.World") +
  theme(panel.background = element_rect(fill = "gray97", colour="gray97"),
        plot.background = element_rect(fill = "gray97", colour="gray97"),
        legend.background = element_rect(fill = "gray97", colour="gray97"),
        legend.key = element_rect(fill = "gray97", colour="gray97"),
        strip.background =element_rect(fill="gray97"),
        strip.text = element_text(colour = 'gray30', family="Bodoni MT", size=16),
        plot.title = element_text(colour = "gray30", size=26, face="bold", hjust = 0.5, family="Bodoni MT Black"),
        plot.subtitle = element_text(colour = "gray30", size=20, hjust = 0.5, family="Bodoni MT"),
        plot.caption = element_text(colour = "gray30", size=16, hjust = 0, family="Bodoni MT"),
        legend.text = element_text(colour = "gray50", size=14, hjust = 0.5, family="Bodoni MT"),
        legend.title = element_text(colour = "gray50", size=16, hjust = 0.5, family="Bodoni MT"),
        legend.position=c(0.9,0.25),
        plot.margin = unit(c(0.3, 20, 0.3, 0.7), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "gray60", size=14, hjust = 0.5, family="Bodoni MT"),
        axis.text = element_text(colour = "gray60", size=14, hjust = 0.5, family="Bodoni MT"),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p


p_inline <- ggplot(nurses, aes(x=`Total Employed RN`, y=`Hourly 50th Percentile`, colour=Year)) +
  geom_point() + labs(x="\nNumber of employed RNs", y="Median hourly wage ($)\n", title="", 
                      subtitle="", caption="") +
  coord_cartesian(expand=F) +
  scale_y_continuous(limits=c(0,60)) +
  scale_x_continuous(limits=c(0,320000), breaks=c(80000,160000,240000,320000), 
                     labels=c(c("80,000","160,000","240,000","320,000"))) +
  scale_colour_gradient("", low="#c6dbef", high="#08519c") +
  theme(panel.background = element_rect(fill = "gray97", colour="gray97"),
        plot.background = element_rect(fill = "gray97", colour="gray97"),
        legend.background = element_rect(fill = "gray97", colour="gray30"),
        strip.background =element_rect(fill="gray97"),
        strip.text = element_text(colour = 'gray30', family="Bodoni MT", size=16),
        plot.title = element_text(colour = "gray30", size=26, face="bold", hjust = 0.5, family="Bodoni MT Black"),
        plot.subtitle = element_text(colour = "gray30", size=20, hjust = 0.5, family="Bodoni MT"),
        plot.caption = element_text(colour = "gray30", size=16, hjust = 0, family="Bodoni MT"),
        legend.text = element_text(colour = "gray60", size=14, hjust = 0.5, family="Bodoni MT"),
        legend.title = element_blank(),
        legend.position=c(0.87, 0.33),
        plot.margin = unit(c(0.3, 0.8, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "gray60", size=14, hjust = 0.5, family="Bodoni MT"),
        axis.text = element_text(colour = "gray60", size=14, hjust = 0.5, family="Bodoni MT"),
        axis.ticks = element_line(colour="gray60"),
        axis.line = element_line(colour="gray60"),
        panel.grid.major = element_line(colour="gray60"),
        panel.grid.minor = element_blank()) 
p_inline

add_plot <- "p_inline.jpeg"
q <- ggdraw() + 
  draw_plot(p) +
  draw_image(add_plot, x = 0.8, y = 0.5, hjust = 0.5, vjust = 1, halign = 0.5, valign = 1, width=0.35) +
  draw_label(x=0.8, y=0.8, hjust=0.5, "HOURLY WAGES OF\nREGISTERED NURSES\n1998 - 2020", color = "gray30", fontface="bold", size = 26, fontfamily="Bodoni MT") +
  draw_label(x=0.8, y=0.6, hjust=0.5, "As of 2020, the median hourly wage of registered nurses in the\nUS is $34.07, compared to $18.48 in 1998. However,\nincreases in hourly wages are mostly seen at\nthe top end of the scale: the 10th percentile pay\nin 1998 was $14.10 and increased to only $25.87 in 2020.\n\nWages may be slowly increasing but the gap\nbetween the highest and lowest paid nurses is widening.\nA higher number of registered nurses is correlated\nwith higher hourly wages.\n\n",
             color = "gray60", size = 16, fontfamily="Bodoni MT") 
q

ggsave(q, filename="05102021.jpg", height=12, width=20, unit="in")


