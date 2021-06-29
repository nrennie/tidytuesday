library(tidyverse)
library(ggdist)
library(ggbump)
library(extrafont)
library(patchwork)
library(cowplot)
tuesdata <- tidytuesdayR::tt_load('2021-06-29')
animal_rescues <- tuesdata$animal_rescues

#process data
rescue_data <- filter(animal_rescues, AnimalGroupParent %in% c("Cat", "Bird", "Dog", "Fox", "Horse"))
rescue_data$AnimalGroupParent <- factor(rescue_data$AnimalGroupParent, levels=c("Cat", "Bird", "Dog", "Fox", "Horse"))
rescue_data$timeofday <- sapply(rescue_data$DateTimeOfCall, function(x) as.numeric(unlist(strsplit(substr(x, start=12, stop=16),":"))[1]) + (as.numeric(unlist(strsplit(substr(x, start=12, stop=16),":"))[2])/60))
r_data <- rescue_data %>% group_by(CalYear) %>% count(AnimalGroupParent)

#rainfall plot
p1 <- ggplot(data=rescue_data, mapping=aes(x = AnimalGroupParent, y = timeofday, fill=AnimalGroupParent, colour=AnimalGroupParent)) +
  stat_halfeye(adjust = .5, width = .5, .width = c(.5, .95)) + 
  stat_dots(side = "left", dotsize = .8, justification = 1.05, binwidth = 0.15) +
  scale_fill_manual("", values=c("#edc951","#800080","#cc2a36","#4f372d","#00a0b0")) +
  scale_colour_manual("", values=c("#edc951","#800080","#cc2a36","#4f372d","#00a0b0")) +
  scale_y_continuous("\nTime of Day", limits=c(0,24), breaks=c(4,12,20), labels=c("04:00", "12:00", "20:00")) +
  labs(x="") +
  coord_flip() +
  theme(plot.background = element_rect(fill = "#fff6e9", colour="#fff6e9"),
        panel.background = element_rect(fill = "#fff6e9", colour="#fff6e9"),
        legend.background = element_rect(fill = "#fff6e9"),
        legend.key = element_rect(fill = "#fff6e9", colour="#fff6e9"), 
        legend.text =  element_text(colour = "#4f372d", size=12, family="Gill Sans MT Condensed"),
        legend.position="none",
        axis.text = element_text(colour = "#4f372d", size=12, family="Gill Sans MT Condensed"),
        axis.title = element_text(colour = "#4f372d", size=12, family="Gill Sans MT Condensed"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1

#geom bump plot
p2 <- ggplot(data=r_data, mapping=aes(x = CalYear, y = n, colour=AnimalGroupParent)) +
  geom_bump() +
  geom_point() + 
  guides(color=guide_legend(nrow=2, label.hjust=0.5)) +
  labs(x="", y="Number of rescues") +
  scale_color_manual("", values=c("#edc951","#800080","#cc2a36","#4f372d","#00a0b0")) +
  theme(plot.background = element_rect(fill = "#fff6e9", colour="#fff6e9"),
        panel.background = element_rect(fill = "#fff6e9", colour="#fff6e9"),
        legend.background = element_rect(fill = "#fff6e9"),
        legend.key = element_rect(fill = "#fff6e9", colour="#fff6e9"), 
        legend.text =  element_text(colour = "#4f372d", size=12, family="Gill Sans MT Condensed"),
        legend.position="top",
        legend.box="vertical",
        axis.text = element_text(colour = "#4f372d", size=12, family="Gill Sans MT Condensed"),
        axis.title = element_text(colour = "#4f372d", size=12, family="Gill Sans MT Condensed"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.minor = element_blank())

p2

#blank plot
p3 <- ggplot(data=r_data, mapping=aes(x = CalYear, y = n, colour=AnimalGroupParent)) +
  labs(x="", y="")  +
  xlim(0,0) + ylim(0,0) +
  theme(plot.background = element_rect(fill = "#fff6e9", colour="#fff6e9"),
        panel.background = element_rect(fill = "#fff6e9", colour="#fff6e9"),
        legend.background = element_rect(fill = "#fff6e9"),
        legend.key = element_rect(fill = "#fff6e9", colour="#fff6e9"), 
        legend.text =  element_text(colour = "#4f372d", size=12, family="Gill Sans MT Condensed"),
        legend.position="top",
        axis.text = element_blank(),
        plot.title = element_text(colour = "black", size=24, face="bold", hjust = 0.5, vjust=-20,family="Gill Sans Ultra Bold"),
        plot.subtitle = element_text(colour = "#4f372d", size=16, hjust = 0.5, family="Gill Sans MT Condensed"),
        plot.caption = element_text(colour = "#4f372d", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3


#join plots together 
p <- (p3 / p2) | p1 & 
  theme(panel.background = element_rect(fill = "#fff6e9", colour="#fff6e9"),
        plot.background = element_rect(fill = "#fff6e9", colour="#fff6e9"))
p

#add titles etc
q <- ggdraw() + 
  draw_plot(p) +
  draw_label(label="Animal\nRescues", x=0.25, y=0.9, hjust=0.5, fontfamily="Gill Sans Ultra Bold", size=24, colour = "#800080") +
  draw_label(label="Cats are the most commonly rescued animal, with\nmore than double the number of rescues as dogs.\nIn 2020, the number of rescues increased\nfor most animals, though not for dogs.\nFewer calls are reported between\n04:00 and 06:00.\n\nN. Rennie | Data: London.gov",
             x=0.25, y=0.75, hjust=0.5, fontfamily="Gill Sans MT Condensed", size=12, colour = "#800080") 


q


dev.new(width=6.5,height=8,unit="in", noRStudioGD = TRUE)
