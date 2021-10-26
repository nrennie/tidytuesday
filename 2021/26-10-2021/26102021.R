library(tidyverse)
library(lubridate)
library(cowplot)
library(magick)
library(grid)
library(ggside)

#read data
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

plot_data <- left_join(ultra_rankings, race, by="race_year_id") %>%
  filter(!is.na(time), 
         rank == 1, 
         runner != "NO Participants") %>%
  mutate(dec_time = hour(hms(time)) + (minute(hms(time))/60) + (second(hms(time))/3600), 
         pace=60*dec_time/distance) %>%
  filter(!is.infinite(pace))

#### plot 1 #### 
p1 <- ggplot(plot_data, mapping=aes(x = age, y =pace)) +
  geom_point(colour="#da225e", size=0.5) +
  geom_xsidedensity(aes(y = stat(density)), fill="#da225e", alpha=0.5) +
  labs(x="Age", y="Pace (min/km)") +
  scale_y_continuous(limits=c(0,NA)) +
  scale_xsidey_continuous(breaks = NULL, labels = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
        plot.background = element_rect(fill = alpha("black", 0.4), colour="transparent"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "white", size=10, hjust = 0.5, family="sans"),
        axis.text = element_text(colour = "white", size=10, hjust = 0.5, family="sans"),
        axis.ticks = element_line(colour="white"),
        panel.grid.major = element_line(colour=alpha("white", 0.1)),
        panel.grid.minor = element_blank()) 
p1

#### plot 2 ####
p2 <- ggplot(data = plot_data, aes(x = elevation_gain, y = pace),) +
  geom_point(colour="#f3bf0d", size=0.5) +
  geom_xsidedensity(aes(y = stat(density)), fill="#f3bf0d", alpha=0.5) +
  labs(x="Elevation Gain (m)", y="Pace (min/km)") +
  scale_xsidey_continuous(breaks = NULL, labels = "") +
  theme_minimal() +
  scale_x_continuous(breaks=c(0,5000, 10000, 15000)) +
  scale_y_continuous(limits=c(0,NA)) +
  theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
        plot.background = element_rect(fill = alpha("black", 0.4), colour="transparent"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "white", size=10, hjust = 0.5, family="sans"),
        axis.text = element_text(colour = "white", size=10, hjust = 0.5, family="sans"),
        axis.ticks = element_line(colour="white"),
        panel.grid.major = element_line(colour=alpha("white", 0.1)),
        panel.grid.minor = element_blank()) 
p2

#### plot 3 ####
p3 <- ggplot(data = plot_data, aes(x = distance^5, y = pace)) +
  geom_point(colour="#539eb5", size=0.5) +
  geom_xsidedensity(aes(y = stat(density)), fill="#539eb5", alpha=0.5) +
  scale_y_continuous(limits=c(0,NA)) +
  scale_x_continuous(breaks=c(100,150,200)^5, labels=c(100,150,200), limits=c(0, 200^5)) +
  labs(x="Distance (km)", y="Pace (min/km)") +
  scale_xsidey_continuous(breaks = NULL, labels = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
        plot.background = element_rect(fill = alpha("black", 0.4), colour="transparent"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "white", size=10, hjust = 0.5, family="sans"),
        axis.text = element_text(colour = "white", size=10, hjust = 0.5, family="sans"),
        axis.ticks = element_line(colour="white"),
        panel.grid.major = element_line(colour=alpha("white", 0.1)),
        panel.grid.minor = element_blank()) 
p3




#### join plots ####
img <- image_read("bg.jpg") 
rect <- rectGrob(
  x = unit(0.05, "npc"),
  y = unit(1, "npc") - unit(0.10, "npc"),
  width = unit(0.9, "npc"),
  height = unit(1.2, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "black", alpha = 0.4)
)

p <- ggdraw() + 
  draw_image(img) +
  draw_grob(rect) +
  draw_plot(p1, .05, .41, .9, .35) +
  draw_plot(p2, .05, .05, .44, .35) +
  draw_plot(p3, .51, .05, .44, .35) +
  #title
  draw_label(x=0.5, y=0.95, hjust=0.5, 
             "Ultra Trail Running", 
             color = "white", size = 24, fontface = "bold", fontfamily="sans") +
  #subtitle
  draw_label(x=0.09, y=0.84, hjust=0, 
             "Different factors such as age, length of race, and elevation gain affect\nhow fast the winning ultrarunners run. The age of the winning runner\nhas little effect on their average pace.", 
             color = "white", size = 12, fontfamily="sans") +
  #caption
  draw_label(x=0.05, y=0.02, hjust=0, 
             "N. Rennie | Data: International Trail Running Association (ITRA) | Background: Sacha T'Sas @ Unsplash", 
             color = "white", size = 8, fontfamily="sans") 
p

ggsave(p, filename="26102021.jpg", height=2880, width=1920, unit="px")
