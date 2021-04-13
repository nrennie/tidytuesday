library(tidyverse)
library(dplyr)
library(maps)
library(magick)
library(extrafont)
tuesdata <- tidytuesdayR::tt_load('2021-04-13')
post_offices <- tuesdata$post_offices
post_offices_ca <- filter(post_offices, state=="CA")
po_ca <- post_offices_ca %>% dplyr::select(c(gnis_latitude, gnis_longitude, established, discontinued))
po_ca <- po_ca %>% drop_na(c(gnis_latitude, gnis_longitude))

select_year <- 1953
d <- filter(po_ca, established <= select_year & (select_year < discontinued | is.na(discontinued)))
states <- map_data("state")
ca <- subset(states, region %in% c("california"))
p <- ggplot() + 
  geom_polygon(data = ca, aes(x = long, y = lat), fill = "#8c96c6", color = "black") +
  geom_point(data=d, aes(x=gnis_longitude, y=gnis_latitude), colour="#6e016b", size=0.7) +
  labs(caption="\nN. Rennie | Data: Harvard Dataverse", title="California Post Offices", 
       subtitle="Data from Cameron Blevins and Richard W. Helbock on US\nPost Offices. Each dot represents the location of an operational\nUS post office in the state of California for the selected year.\nThe number of post offices increased steeply until the 1910s\nbefore declining steadily.") +
  annotate("text", x=-123.5, y=34, label=select_year, colour = "#6e016b", family="Corbel Light", fontface=2, size=10) +
  theme(axis.text=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        plot.background = element_rect(fill = "#e0ecf4", color = NA),
        panel.background = element_rect(fill = "#e0ecf4"),
        plot.margin = unit(c(0.5, 0.5, 0.3, 0.3), "cm"),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(1,1),legend.justification=c(1,1),
        plot.title = element_text(colour = "#6e016b", size=18, face="bold", hjust=0, family="Corbel Light"),
        plot.subtitle = element_text(colour = "#6e016b", size=10, hjust=0, family="Corbel Light"),
        plot.caption = element_text(colour = "#6e016b", size = 8, hjust=1, family="Corbel Light"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.title=element_blank(),
        legend.key = element_blank())
p
ggsave(p, filename = "13042021.jpg", height=5, width=4, unit="in")

#make animation
all_years <- 1849:1983
make_plot <- function(select_year){
  d <- filter(po_ca, established <= select_year & (select_year < discontinued | is.na(discontinued)))
  states <- map_data("state")
  ca <- subset(states, region %in% c("california"))
  p <- ggplot() + 
    geom_polygon(data = ca, aes(x = long, y = lat), fill = "#8c96c6", color = "black") +
    geom_point(data=d, aes(x=gnis_longitude, y=gnis_latitude), colour="#6e016b", size=0.7) +
    labs(caption="\nN. Rennie | Data: Harvard Dataverse", title="California Post Offices", 
         subtitle="Data from Cameron Blevins and Richard W. Helbock on US\nPost Offices. Each dot represents the location of an operational\nUS post office in the state of California for the selected year.\nThe number of post offices increased steeply until the 1910s\nbefore declining steadily.") +
    annotate("text", x=-123.5, y=34, label=select_year, colour = "#6e016b", family="Corbel Light", fontface=2, size=10) +
    theme(axis.text=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title=element_blank(),
          plot.background = element_rect(fill = "#e0ecf4", color = NA),
          panel.background = element_rect(fill = "#e0ecf4"),
          plot.margin = unit(c(0.5, 0.5, 0.3, 0.3), "cm"),
          legend.background = element_rect(color = NA,fill="transparent"),
          legend.box.background = element_rect(fill = "transparent",color=NA),
          legend.position=c(1,1),legend.justification=c(1,1),
          plot.title = element_text(colour = "#6e016b", size=18, face="bold", hjust=0, family="Corbel Light"),
          plot.subtitle = element_text(colour = "#6e016b", size=10, hjust=0, family="Corbel Light"),
          plot.caption = element_text(colour = "#6e016b", size = 8, hjust=1, family="Corbel Light"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          legend.title=element_blank(),
          legend.key = element_blank())
  p
  ggsave(p, filename = paste(select_year, ".jpg"), height=5, width=4, unit="in")
}

sapply(all_years, function(x) make_plot(x))

number_offices <- function(select_year){
  d <- filter(po_ca, established <= select_year & (select_year < discontinued | is.na(discontinued)))
  return(nrow(d))
}
n <- sapply(all_years, function(x) number_offices(x))

#make gif
imgs <- list.files(full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
anim <- image_animate(image_scale(img_joined,"600x750"), fps = 10) 
image_write(image = anim,path = "13042021.gif")


