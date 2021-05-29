library(tidyverse)
library(extrafont)
library(cowplot)
library(magick)

dev.new(width=5,height=7,unit="in", noRStudioGD = TRUE)

tuesdata <- tidytuesdayR::tt_load('2021-06-01')
summary <- tuesdata$summary

plot_data <- summary %>% select(season_name, season, viewers_premier, viewers_finale)
plot_data$viewers_premier <- -1*plot_data$viewers_premier
long_data <- pivot_longer(plot_data, c(viewers_premier, viewers_finale), values_to = "viewers", names_to="episode")

p <- ggplot() +  
  geom_bar(data=long_data, aes(x = season, y = viewers, fill = episode), stat = "identity", width = .8) +
  geom_text(data=data.frame(x=1, y=-55, label="Season 1"), mapping=aes(x=x, y=y, label=label), colour="white", size=3, family="Ebrima", hjust=0.5) +
  geom_text(data=data.frame(x=40, y=-55, label="Season 40"), mapping=aes(x=x, y=y, label=label), colour="white", size=3, family="Ebrima", hjust=0.5) +
  geom_segment(data=long_data, aes(x = 3, y = -55, xend = 38, yend = -55), colour="white", arrow=arrow(length=unit(0.30,"cm"), type = "closed")) +
  coord_flip() +
  scale_fill_manual("", values=c("viewers_finale"="#00ab50", "viewers_premier"="#358bbd"), breaks=c("viewers_finale","viewers_premier"), labels=c("Finale", "Premier")) +
  scale_x_reverse() +
  scale_y_continuous(limits=c(-60, 55), breaks=c(-50,-25,0,25,50), labels=c("50", "25", "0", "25", "50")) +
  labs(title="Survivor: Viewership", caption="\nN. Rennie | Data: survivorR",
       subtitle="Viewership for Survivor has been slowly declining across\n40 seasons. On average, more people watch the finale\nthan the premier.",
       y="\nNumber of viewers (millions)", x="") +
  theme(panel.background = element_rect(fill = "black", colour="black"),
        plot.background = element_rect(fill = "black", colour="black"),
        legend.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white", size=20, face="bold", hjust = 0, family="Berlin Sans FB"),
        plot.subtitle = element_text(colour = "white", size=10, hjust = 0, family="Ebrima"),
        plot.caption = element_text(colour = "white", size=10, hjust = 0, family="Ebrima"),
        legend.position=c(0.8,0.2),
        legend.key = element_rect(colour = "black", fill="black"),
        plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.spacing.x = unit(0.5,"cm"),
        legend.title = element_text(colour = "white", size=10, hjust = 0.5, family="Ebrima"),
        legend.text = element_text(colour="white", size=10, family="Ebrima", hjust = 0),
        axis.title.y= element_blank(),
        axis.title.x= element_text(colour = "white", size=10, family="Ebrima"),
        axis.text.y=element_blank(),
        axis.text.x=element_text(colour = "white", size=10, hjust = 1, family="Ebrima"),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )
p

#save images along the way
fname <- paste(gsub(":", "-",gsub(" ", "", Sys.time(), fixed = TRUE)), ".jpg", sep="")
ggsave(p, filename = fname,  bg = "transparent", height=7, width=5, unit="in")

#add logo
logo_file <- "./logo.png"
q <- ggdraw() + 
  draw_plot(p) +
  draw_image(
    logo_file, x = 0.81, y = 0.45, hjust = 0.5, vjust = 1, halign = 1, valign = 1,
    width = 0.3
  )
q

#save final image
ggsave(q, filename = "29052021.jpg",  bg = "transparent", height=7, width=5, unit="in")

#animate gif of progress
imgs <- list.files(full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
anim <- image_animate(image_scale(img_joined,"480x672"), fps = 1)
image_write(image = anim,path = "01062021_tt.gif")

#animate pyramid map
make_pyramid <- function(season_val){
  d <- filter(long_data, season <= season_val)
  seg_y <- c(NA, (43-season_val))[(as.numeric(season_val > 6) + 1)]
  seg_y_end <- c(NA, 38)[(as.numeric(season_val > 6) + 1)]
  lab_y <- c((40-season_val), NA)[(as.numeric(season_val == 1) + 1)]
  p <- ggplot() +  
    geom_bar(data=d, aes(x = season+(40-season_val), y = viewers, fill = episode), stat = "identity", width = .8) +
    geom_text(data=data.frame(x=lab_y, y=-55, label="Season 1"), mapping=aes(x=x, y=y, label=label), colour="white", size=3, family="Ebrima", hjust=0.5) +
    geom_text(data=data.frame(x=40, y=-55, label=paste("Season ", season_val, sep="")), mapping=aes(x=x, y=y, label=label), colour="white", size=3, family="Ebrima", hjust=0.5) +
    geom_segment(data=long_data, aes(x = seg_y, y = -55, xend = seg_y_end, yend = -55), colour="white", arrow=arrow(length=unit(0.30,"cm"), type = "closed")) +
    coord_flip() +
    scale_fill_manual("", values=c("viewers_finale"="#00ab50", "viewers_premier"="#358bbd"), breaks=c("viewers_finale","viewers_premier"), labels=c("Finale", "Premier")) +
    scale_x_reverse(limits=c(41,0)) +
    scale_y_continuous(limits=c(-60, 55), breaks=c(-50,-25,0,25,50), labels=c("50", "25", "0", "25", "50")) +
    labs(title="Survivor: Viewership", caption="\nN. Rennie | Data: survivorR",
         subtitle="Viewership for Survivor has been slowly declining across\n40 seasons. On average, more people watch the finale\nthan the premier.",
         y="\nNumber of viewers (millions)", x="") +
    theme(panel.background = element_rect(fill = "black", colour="black"),
          plot.background = element_rect(fill = "black", colour="black"),
          legend.background = element_rect(fill = "black"),
          plot.title = element_text(colour = "white", size=20, face="bold", hjust = 0, family="Berlin Sans FB"),
          plot.subtitle = element_text(colour = "white", size=10, hjust = 0, family="Ebrima"),
          plot.caption = element_text(colour = "white", size=10, hjust = 0, family="Ebrima"),
          legend.position="top",
          legend.key = element_rect(colour = "black", fill="black"),
          plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
          legend.spacing.x = unit(0.5,"cm"),
          legend.title = element_text(colour = "white", size=10, hjust = 0.5, family="Ebrima"),
          legend.text = element_text(colour="white", size=10, hjust = 0),
          axis.title.y= element_blank(),
          axis.title.x= element_text(colour = "white", size=10, family="Ebrima"),
          axis.text.y=element_blank(),
          axis.text.x=element_text(colour = "white", size=10, hjust = 1, family="Ebrima"),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
  q <- ggdraw() + 
    draw_plot(p) +
    draw_image(
      logo_file, x = 0.85, y = 0.95, hjust = 0.5, vjust = 1, halign = 1, valign = 1,
      width = 0.21
    )
  q
  return(q)
}

#make_pyramid(20)

for(i in 1:40){
  k <- make_pyramid(i)
  ggsave(k, filename = paste(i, ".jpg", sep=""),  bg = "transparent", height=7, width=5, unit="in")
}

#animate gif of progress
imgs <- list.files(full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
anim <- image_animate(image_scale(img_joined,"480x672"), fps = 4)
image_write(image = anim,path = "01062021.gif")
