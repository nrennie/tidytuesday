library(tidyverse)
library(ggpol)
library(grid)
library(gridExtra)
library(extrafont)
library(cowplot)
library(ghibli)

tuesdata <- tidytuesdayR::tt_load('2021-08-31')
bird_baths <- tuesdata$bird_baths



#prep data
choose_birds <- unlist((bird_baths %>% group_by(bird_type) %>% summarise(n = sum(bird_count)) %>% arrange(desc(n)))[1:5,1])
plot_data <- bird_baths %>% filter(bird_type %in% choose_birds & !is.na(bioregions) & !is.na(urban_rural)) %>% group_by(bird_type, bioregions, urban_rural) %>% summarise(n = sum(bird_count))

#plot
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

p1 <- ggplot(filter(plot_data, urban_rural == "Rural"), aes(x = bioregions, y = -n, fill = bird_type)) +
  geom_bar(stat = "identity", position="stack", key_glyph = "polygon3") +
  labs(x="", y="") +
  coord_flip() +
  scale_fill_manual("", values=ghibli_palettes$PonyoMedium[1:6]) +
  scale_y_continuous(limits=c(-500,0), breaks=c(0,-250,-500), labels=c(0,250, 500)) +
  theme_minimal() +
  guides(fill=guide_legend(ncol=1)) +
  theme(plot.background = element_rect(fill = "#E8C4A2", colour="#E8C4A2"),
        panel.background = element_rect(fill = "#E8C4A2", colour="#E8C4A2"),
        legend.background = element_rect(fill = "#E8C4A2", colour="#E8C4A2"),
        legend.position=c(0.2,0.5),
        legend.title = element_blank(),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(colour = "#4C413F", size=14, hjust = 0.5, family="Gill Sans MT Condensed"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "#4C413F", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        axis.title = element_text(colour = "#4C413F", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        plot.margin = unit(c(2.5, 0, 0.5, 0), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1

p2 <- ggplot(filter(plot_data, urban_rural == "Urban"), aes(x = bioregions, y = n, fill = bird_type)) +
  geom_bar(stat = "identity", position="stack") + 
  coord_flip() +
  scale_fill_manual("", values=ghibli_palettes$PonyoMedium[1:6]) +
  scale_y_continuous(limits=c(0,500), breaks=c(0,250,500)) +
  labs(x="", y="") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#E8C4A2", colour="#E8C4A2"),
        panel.background = element_rect(fill = "#E8C4A2", colour="#E8C4A2"),
        legend.background = element_rect(fill = "#E8C4A2", colour="#E8C4A2"),
        legend.position="none",
        legend.key = element_rect(fill="#E8C4A2"),
        legend.text = element_text(colour = "#4C413F", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "#4C413F", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        axis.title = element_text(colour = "#4C413F", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        plot.margin = unit(c(2.5, 0, 0.5, 0), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2

#join plots
g.mid<-ggplot(plot_data,aes(x=1,y=bioregions)) + 
  geom_text(aes(label=bioregions), vjust=-1.5, hjust=0.5, color = "#4C413F", size = 5, family="Gill Sans MT Condensed")+
  ggtitle("")+
  labs(y="") +
  scale_x_continuous(expand=c(0,0),limits=c(0.92,1.06))+
  theme(plot.background = element_rect(fill = "#E8C4A2", colour="#E8C4A2"),
        panel.background = element_rect(fill = "#E8C4A2", colour="#E8C4A2"),
        legend.background = element_rect(fill = "#E8C4A2", colour="#E8C4A2"),
        legend.position="none",
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(2.5, 0, 0.5, 0), "cm"), #top, right, bottom, left
        #plot.margin = unit(c(1,-1,1,-1), "mm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
gg1 <- ggplot_gtable(ggplot_build(p1))
gg2 <- ggplot_gtable(ggplot_build(p2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))
p <- grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/11,3/11,4/11))
p

#add bird image
bird <- "./images/bird.png"
q <- ggdraw() + 
  draw_plot(p) +
  draw_image(bird, x = 0.85, y = 0.475, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.3) +
  draw_label(x=0.2, y=0.85, hjust=0.5, "RURAL", color = "#5A6F80", size = 18, fontfamily="Gill Sans MT Condensed") +
  draw_label(x=0.8, y=0.85, hjust=0.5, "URBAN", color = "#5A6F80", size = 18, fontfamily="Gill Sans MT Condensed") +
  draw_label(x=0.52, y=0.95, hjust=0.5, "Australian Birds", color = "#4C413F", size = 28, fontfamily="Gill Sans Ultra Bold") +
  draw_label(x=0.52, y=0.9, hjust=0.5, "Sightings of the five most populous birds in Australia show that, of the five, most are spotted in urban areas of the Sydney Basin.", 
             color = "#4C413F", size = 16, fontfamily="Gill Sans MT Condensed") +
  draw_label(x=0.52, y=0.03, hjust=0.5, "N. Rennie | Data: Cleary et al. (2016)", color = "#4C413F", size = 12, fontfamily="Gill Sans MT Condensed") 

q

