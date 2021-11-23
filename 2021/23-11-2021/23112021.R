library(tidyverse)
library(ggalt)
library(cowplot)
library(magick)
library(grid)

tuesdata <- tidytuesdayR::tt_load('2021-11-23')
episodes <- tuesdata$episodes 
plot_data <- episodes %>%
  filter(!is.na(season_number),
         !is.na(rating),
         !is.na(uk_viewers), 
         season_number <=12) %>%
  mutate(season = factor(season_number, levels=1:12, labels=paste(rep("Season", 12), 1:12))) %>%
  group_by(season) %>%
  summarise(min=min(uk_viewers), 
            max=max(uk_viewers))


#plot graph
p <- ggplot() +
  geom_dumbbell(data=plot_data, mapping=aes(x=min, xend=max, y=season), 
                colour_x = "#a6b8c7", colour_xend = "#6f8ea9", color="white", size=1, size_x = 3, size_xend = 3) +
  labs(x="\nUK viewers (millions)", y="") +
  theme(panel.background = element_rect(fill = "transparent", colour =  "transparent"),
        plot.background = element_rect(fill =  alpha("black", 0.6), colour =  alpha("black", 0.5)),
        legend.position="none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        plot.title = element_text(colour = "white", size=18, face="bold", family="serif"),
        plot.subtitle = element_text(colour = "white", size=12, family="serif"),
        plot.caption = element_text(colour = "white", size=12, family="serif"),
        legend.title = element_text(colour = "white", size=12, family="serif"),
        axis.title= element_text(colour="white", size=12, family="serif"),
        axis.text= element_text(colour="white", size=12, family="serif"),
        axis.ticks.x = element_line(colour="white"),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p

#add bg image
img <- image_read("bg.jpg")
rect <- rectGrob(
  x = unit(0.05, "npc"),
  y = unit(1, "npc") - unit(0.05, "npc"),
  width = unit(0.46, "npc"),
  height = unit(1.5, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "black", alpha = 0.6)
)
q <- ggdraw() + 
  draw_image(img) +
  draw_grob(rect) +
  draw_plot(p, .55, .05, .4, .9) +
  #add label to plot
  draw_label(x=0.67, y=0.92, hjust=0, 
             "Min      Max", 
             color = "white", size = 12, fontfamily="serif") +
  #title
  draw_label(x=0.07, y=0.91, hjust=0, 
             "Who is watching Doctor Who?", 
             color = "white", size = 20, fontface = "bold", fontfamily="serif") +
  #subtitle
  draw_label(x=0.07, y=0.82, hjust=0, 
             "The number of people watching the revival of Doctor Who has been steadily\ndeclining in recent years. On Christmas Day 2007, Voyage of the Damned\nbecame the most watched episode with 13.3 million people tuning in.\nJodie Whittaker's first series as the Doctor brought back an increase in\nviewership.", 
             color = "white", size = 10, fontfamily="serif") +
  #caption
  draw_label(x=0.05, y=0.03, hjust=0, 
             "N. Rennie | Data: datardis | Image: Nick Fewings @ Unsplash", 
             color = "white", size = 8, fontfamily="serif") 
q

ggsave(q, filename="23112021.jpg")


