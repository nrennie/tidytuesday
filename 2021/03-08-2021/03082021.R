library(tidyverse)
library(extrafont)
library(patchwork)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load('2021-08-03')
athletes <- tuesdata$athletes

#plot dumbbell plot
plot_data_1 <- athletes %>% filter(abb %in% c("GBR", "USA") & !is.na(gender)) %>% group_by(type, gender, abb) %>% summarise(total_medals = n())
p1 <- ggplot() + 
  geom_line(plot_data_1, mapping=aes(x=reorder(type, total_medals), y=total_medals, col=gender, group = type), colour = "#005085")+
  geom_point(plot_data_1, mapping=aes(x=reorder(type, total_medals), y=total_medals, col=gender, group = type), size=3) + 
  scale_colour_manual("", values=c("Men"="#005085", "Women"="#da0019", "Mixed"="#800080")) +
  labs(x="", y="\nNumber of medals") +  
  facet_grid(~abb) +
  coord_flip() +
  ylim(0, 600) +
  theme(plot.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        panel.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        legend.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        legend.key = element_rect(fill = NA),
        strip.background =element_rect(fill=alpha("#005085",0.2)),
        strip.text = element_text(colour = '#005085', family="Gill Sans MT", size=12),
        legend.position="bottom",
        legend.margin=margin(t = 0, unit='cm'),
        axis.ticks.y = element_blank(),
        axis.text = element_text(colour = "#005085", size=10, hjust = 0.5, family="Gill Sans MT"),
        axis.title = element_text(colour = "#005085", size=12, hjust = 0.5, family="Gill Sans MT"),
        legend.text = element_text(colour = "#005085", size=10, hjust = 0.5, family="Gill Sans MT"),
        plot.title = element_text(colour = "#005085", size=28, hjust = 0, family="Gill Sans MT"),
        plot.subtitle = element_text(colour = "#005085", size=14, hjust = 0, family="Gill Sans MT"),
        plot.caption = element_text(colour = "#005085", size=12, hjust = 1, family="Gill Sans MT"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), #top, right, bottom, left
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size=0.1, colour=alpha("#005085",0.4)),
        panel.grid.minor = element_blank())
p1

#plot heatmap of medals
plot_data_2 <- athletes %>% filter(abb %in% c("GBR", "USA")) %>% group_by(type, year, abb) %>% summarise(total_medals = n()) 
plot_data_2$type <- factor(plot_data_2$type, levels=c("Triathlon", "Fencing", "Archery", "Wheelchair Tennis", "Table Tennis", "Basketball", "Cycling", "Volleyball", "Rugby", "Athletics", "Swimming"))
p2 <- ggplot(plot_data_2, aes(x=year, y=type, fill= total_medals)) + 
  geom_tile() +
  labs(x="", y="") +
  facet_grid(~abb) +
  scale_x_continuous(breaks=seq(1980, 2018, 4)) +
  scale_fill_gradient("Number of medals", low="#d3eeff", high="#005085", limits=c(0,250), na.value="#d3eeff") +
  guides(fill = guide_colorbar(title.position = "top")) +
  theme(plot.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        strip.background =element_rect(fill=alpha("#005085",0)),
        strip.text = element_text(colour = "transparent", family="Gill Sans MT", size=12),
        panel.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        legend.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        legend.position="bottom",
        legend.key.width = unit(1, "cm"),
        legend.margin=margin(t = 0, unit='cm'),
        axis.ticks.y = element_blank(),
        axis.text = element_text(colour = "#005085", size=10, hjust = 0.5, family="Gill Sans MT"),
        axis.title = element_text(colour = "#005085", size=12, hjust = 0.5, family="Gill Sans MT"),
        legend.text = element_text(colour = "#005085", size=10, hjust = 0.5, family="Gill Sans MT"),
        legend.title = element_text(colour = "#005085", size=12, hjust = 0.5, family="Gill Sans MT"),
        plot.title = element_text(colour = "#005085", size=28, hjust = 0, family="Gill Sans MT"),
        plot.subtitle = element_text(colour = "#005085", size=14, hjust = 0, family="Gill Sans MT"),
        plot.caption = element_text(colour = "#005085", size=12, hjust = 1, family="Gill Sans MT"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2

#combine plots
p <- p1 + p2 + plot_layout(nrow=2, ncol=1) + 
  plot_annotation(title = "Team GB v Team USA", 
                  caption = "N. Rennie | Data*: International Paralympic Committee\n*1980-2016", 
                  subtitle="Athletics has generated the most paralympic medals for both Teams USA and GB,\nit's also where the largest gender disparity in the number of medals occurs.\nMost medals were won in 1984, when the two countries jointly hosted the\nParalympic Summer Games.") &
  theme(plot.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        panel.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        plot.title = element_text(colour = "#005085", size=28, hjust = 0.5, family="Gill Sans MT"),
        plot.subtitle = element_text(colour = "#005085", size=12, hjust = 0.5, family="Gill Sans MT"),
        plot.caption = element_text(colour = "#005085", size=12, hjust = 0, family="Gill Sans MT"))
p

dev.new(width=10,height=9,unit="in", noRStudioGD = TRUE)

#add logos
gb_logo <- "./images/gb_logo.png"
us_logo <- "./images/us_logo.png"

q <- ggdraw() + 
  draw_plot(p) +
  draw_image(gb_logo, x = 0.1, y = 0.99, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.08) +
  draw_image(us_logo, x = 0.9, y = 0.98, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.19)
q
