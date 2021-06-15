library(tidyverse)
library(rnaturalearth)
library(extrafont)
library(gganimate)

tuesdata <- tidytuesdayR::tt_load('2021-06-15')
tweets <- tuesdata$tweets

tweets_data <- filter(tweets, !is.na(lat) & !is.na(long) & !is.na(verified))
tweets_data <- tweets_data[order(tweets_data$datetime),]
tweets_data$obs <- factor(1:355)

world <- ne_countries(scale = "medium", returnclass = "sf")
world_data <- filter(world, continent != "Antarctica")
p <- ggplot() +
  geom_sf(data = world_data, fill= "#F5F8FA") + 
  geom_point(data = tweets_data, mapping=aes(x=long, y=lat, size=like_count, colour=username)) +
  guides(color = FALSE) +
  scale_size("Number of Likes", range = c(1,8)) +
  labs(title="#DuBoisChallenge\n", subtitle="The DuBois Challenge celebrated \"the data visualization legacy of W.E.B DuBois by recreating the\nvisualizations from the 1900 Paris Exposition using modern tools.\" These are the tweets from around\nthe globe using #DuBoisChallenge, showing different users and number of likes.", caption="N. Rennie | Data: Anthony Starks, Allen Hillery & Sekou Tyler") +
  theme(plot.background = element_rect(fill = "#E1E8ED", colour="#E1E8ED"),
        panel.background = element_rect(fill = "#E1E8ED", colour="#E1E8ED"),
        legend.background = element_rect(fill = "#E1E8ED"),
        legend.key = element_rect(fill = "#E1E8ED", colour="#E1E8ED"), 
        legend.text =  element_text(colour = "#657786", size=12, family="Segoe UI"),
        legend.title =  element_text(colour = "#657786", size=12, family="Segoe UI", hjust=0.5),
        plot.title = element_text(colour = "#1DA1F2", size=24, face="bold", hjust = 0.5, family="Segoe UI"),
        plot.subtitle = element_text(colour = "#657786", size=14, hjust = 0.5, family="Segoe UI"),
        plot.caption = element_text(colour = "#657786", size=12, hjust = 0.5, family="Segoe UI"),
        legend.position="bottom",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank())
p
dev.new(width=11.1,height=7,unit="cm", noRStudioGD = TRUE)
ggsave(p, filename = "15062021.jpg", width=11.1,height=7,unit="in",)

anim <- p +
  transition_states(obs, transition_length = 4, state_length = 1) +
  enter_fade() +
  exit_fade() + shadow_mark()
animate(anim, nframes = 355, height = 7, width = 11.1, units = "in", res=150, fps=20)
anim_save("15062021.gif", animation = last_animation())
