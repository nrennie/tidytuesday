library(tidyverse)
devtools::install_github("ricardo-bion/ggradar")
library(ggradar) 
devtools::install_github("johnmackintosh/popthemes")
library(popthemes)

# load data
tuesdata <- tidytuesdayR::tt_load('2021-12-14')
studio_album_tracks <- tuesdata$studio_album_tracks

# prep data
plot_data <- studio_album_tracks %>%
  select(album_name, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence) %>%
  group_by(album_name) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

# make base plot
plot_data %>%
  ggradar()

# make plot
p <- plot_data %>%
  ggradar(font.radar = "serif", 
          axis.labels = str_to_title(colnames(plot_data)[-1]),
          axis.line.colour = "#AA336A",
          gridline.min.colour = "#AA336A",
          gridline.mid.colour = "#AA336A",
          gridline.max.colour = "#AA336A",
          background.circle.colour = "#AA336A",
          group.point.size=3,
          grid.label.size=0) +
  scale_colour_spice() +
  labs(title = "Spice Up Your Life!", 
       tag = str_wrap("All three Spice Girls albums are high in valence, danceability, and energy. Spice has both the highest danceability and the highest instrumentalness.", 30),
       caption = "N. Rennie | Data: Spotify & Genius") +
  theme(plot.background = element_rect(fill = "#FFE6EE", colour="#FFE6EE"),
        panel.background = element_rect(fill = "#FFE6EE", colour="#FFE6EE"),
        plot.title = element_text(colour = "#AA336A", size=22, face="bold", family="serif", hjust=-0.85, vjust=-20),
        plot.tag = element_text(colour = "#AA336A", size=16, family="serif"),
        plot.caption = element_text(colour = "#AA336A", size=12, family="serif", hjust=-0.73),
        plot.tag.position = c(-0.25, 0.5),
        legend.background = element_rect(fill = "#FFE6EE", colour="#FFE6EE"),
        legend.key = element_rect(fill = "#FFE6EE", colour="#FFE6EE"), 
        legend.text =  element_text(colour = "#AA336A", size=12, family="serif"),
        legend.position="bottom",
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "#AA336A", size=16, family="serif"),
        axis.title = element_text(colour = "#AA336A", size=16, family="serif"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 9), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# save plot
ggsave(p, filename="14122021.jpg")

