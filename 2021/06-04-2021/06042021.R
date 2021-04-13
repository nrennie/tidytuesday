library(tidyverse)
library(extrafont)
library(gganimate)
library(transformr)
library(magick)
library(sf)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load('2021-04-06')
soybean_use <- tuesdata$soybean_use

soybean_use_total <- filter(soybean_use, entity == "World")

p1 <- ggplot() +
  geom_point(data = soybean_use_total, mapping = aes(x = year, y = human_food, group=year),colour = "navy") +
  geom_line(data = soybean_use_total, mapping = aes(x = year, y = human_food, group=1), colour = "navy") +  
  coord_cartesian(expand=F) +
  scale_y_continuous(name="Production (tonnes)", labels = scales::label_number_si(), limits=c(0,12000000)) +
  labs(caption="\nN. Rennie | Data: Our World in Data", title="Global Soy Production", 
       subtitle="The production of soy for the purposes of human food e.g. tofu has\nincreased by 250% since 1960. However, current production levels\nare similar to those in the late 1990s.\n") +
  theme(axis.text=element_text(family="Times New Roman", size=9),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 2, b = 0, l = 0)),
        axis.title=element_text(size=9, colour="navy"),
        legend.text=element_text(size=9, colour="navy"),
        plot.background = element_rect(fill = "azure", color = NA),
        panel.background = element_rect(fill = "azure"),
        plot.margin = unit(c(0.5, 0.5, 0.3, 0.3), "cm"),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(1,1),legend.justification=c(1,1),
        plot.title = element_text(colour = "navy", size=12, face="bold", hjust=0),
        plot.subtitle = element_text(colour = "navy", size=8, hjust=0),
        plot.caption = element_text(colour = "navy", size = 8, hjust=0),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="navy"),
        axis.line.x = element_line(size=.1, color="navy"),
        axis.line.y = element_line(size=.1, color="navy"),
        legend.title=element_blank(),
        legend.key = element_blank())
p1
ggsave(p1, filename = "06042021_plot1.jpg", height=4, width=4, unit="in")

a1 <- p1 + transition_reveal(year) 
anim1 <- animate(a1, nframes = 53, fps=10, height = 4, width = 4, units = "in", res=150)
anim1
anim_save("06042021_anim1.gif", animation = last_animation())

#############################################################################################################################
#############################################################################################################################

#download shapefile from https://hub.arcgis.com/datasets/57c1ade4fa7c4e2384e6a23f2b3bd254_0?geometry=11.953%2C-89.382%2C-11.953%2C86.054
shp <- st_read("4a7d27e1-84a3-4d6a-b4c2-6b6919f3cf4b202034-1-2zg7ul.ht5ut.shp")
shp2 <- shp[c(1,2,3,8,4,5,6),] #remove antarctica and sort

continents <- c("Africa", "Asia", "Australia","Europe", "Northern America", "Oceania", "South America")
soybean_use_continents <- filter(soybean_use, entity %in% continents)

make_plot <- function(select_year){
  shp2$soy <- filter(soybean_use_continents, year == select_year)$human_food
  p2 <- ggplot() + geom_sf(data=shp2, aes(fill=soy), size=0.3) +
    scale_fill_continuous(" Production (tonnes)", low="white", high="navy", 
                          limits=c(0, 10000000), breaks=c(0, 5000000, 10000000), 
                          labels = c("0", "5M", "10M")) +
    guides(fill = guide_colorbar(title.position = "top")) +
    annotate("text", x=0, y=-60, label=select_year, colour="navy") +
    theme(axis.text=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(1.8, 0, 1.8, 0), "cm"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.text=element_text(size=9),
          plot.background = element_rect(fill = "azure", color = NA),
          panel.background = element_rect(fill = "azure"),
          legend.background = element_rect(color = NA,fill="transparent"),
          legend.box.background = element_rect(fill = "transparent",color=NA),
          legend.position="top",
          axis.ticks = element_blank(),
          legend.title=element_text(size=9),
          legend.key = element_blank())
  save_plot(paste("p_",select_year,".jpg", sep=""), p2, base_height = 4, base_width = 4)
}
select_year <- 2013
shp2$soy <- filter(soybean_use_continents, year == select_year)$human_food
p2 <- ggplot() + geom_sf(data=shp2, aes(fill=soy), size=0.3) +
  scale_fill_continuous(" Production (tonnes)", low="white", high="navy", 
                        limits=c(0, 10000000), breaks=c(0, 5000000, 10000000), 
                        labels = c("0", "5M", "10M")) +
  guides(fill = guide_colorbar(title.position = "top")) +
  annotate("text", x=0, y=-60, label=select_year, colour="navy") +
  theme(axis.text=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1.8, 0, 1.8, 0), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text=element_text(size=9),
        plot.background = element_rect(fill = "azure", color = NA),
        panel.background = element_rect(fill = "azure"),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="top",
        axis.ticks = element_blank(),
        legend.title=element_text(size=9),
        legend.key = element_blank())
p2
ggsave(p2, filename = "06042021_plot2.jpg", height=4, width=4, unit="in")

sapply(unique(soybean_use_continents$year), function(x) make_plot(x))

#make gif
imgs <- list.files(full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
anim2 <- image_animate(image_scale(img_joined,"600x600"), fps = 10)
image_write(image = anim2,path = "06042021_anim2.gif")

#############################################################################################################################
#############################################################################################################################

#combine images
p <- plot_grid(p1, p2, ncol=2, nrow=1)
p
ggsave(p, filename = "06042021.jpg", height=4, width=8, unit="in")

#############################################################################################################################
#############################################################################################################################

#combine gifs
a_mgif <- image_read(anim1)
b_mgif <- anim2

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:53){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}
image_write(image = new_gif,path = "06042021.gif")

