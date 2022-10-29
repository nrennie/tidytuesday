library(tidyverse)
library(showtext)
library(emojifont)
library(camcorder)
library(PrettyCols)
library(cowplot)

# load data
bakers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/bakers.csv')
challenges <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/challenges.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/episodes.csv')
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/ratings.csv')

# data wrangling
winners <- bakers %>% 
  filter(series_winner == 1) 

num_stars <- episodes %>% 
  group_by(sb_name) %>% 
  summarise(stars = n()) %>% 
  filter(sb_name %in% winners$baker)

plot_data <- winners %>% 
  left_join(num_stars, by = c("baker" = "sb_name")) %>% 
  select(series, baker, technical_median, stars) %>% 
  mutate(stars = replace_na(stars, 0)) %>% 
  mutate(y = c(rep(1, 4), rep(2, 3), rep(3, 2), 4), 
         x = c(-1.5, -0.5, 0.5, 1.5, -1, 0, 1, -0.5, 0.5, 0))

# data for legend
legend_data <- data.frame(x = rep(3, 4), 
                          y = c(1, 1.25, 1.5, 1.75), 
                          stars = 0:3, 
                          label = c("0 Star Bakers", "1 Star Baker Awards", "2 Star Baker Awards", "3 Star Baker Awards"))

# subtitle
st <- usefunc::str_wrap_break("Series 10 winner David became the first winner never to have won the Star Baker title since it was introduced in series 2.", 35)

# start recording
gg_record(
  dir = file.path("2022", "2022-10-25", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

#plot
stand <- ggplot(plot_data) +
  # cake stand vertical
  geom_segment(mapping = aes(x = 0, xend = 0, y = 0, yend = 5),
               size = 4, 
               colour = "lightgray") +
  geom_point(mapping = aes(x = 0, y = 5),
             size = 7,
             colour = "#cdcdcd") +
  # cake stand horizontal
  geom_segment(mapping = aes(x = -0.5, xend = 0.5, y = 0, yend = 0),
               size = 7, 
               colour = "#cdcdcd") +
  geom_segment(mapping = aes(x = -1.9, xend = 1.9, y = 0.8, yend = 0.8),
               size = 7, 
               colour = "#cdcdcd") +
  geom_segment(mapping = aes(x = -1.4, xend = 1.4, y = 1.8, yend = 1.8),
               size = 7, 
               colour = "#cdcdcd") +
  geom_segment(mapping = aes(x = -0.9, xend = 0.9, y = 2.8, yend = 2.8),
               size = 7, 
               colour = "#cdcdcd") +
  geom_segment(mapping = aes(x = -0.4, xend = 0.4, y = 3.8, yend = 3.8),
               size = 7, 
               colour = "#cdcdcd")

# add cakes
cakes <- stand +
  geom_text(mapping = aes(x = x, y = y, colour = factor(stars), label = emoji("cake")),
            family = "EmojiOne",
            size = 45, 
            lineheight = 0.05) +
  geom_label(mapping = aes(x = x, y = y - 0.2, label = baker),
             size = 9) +
  scale_y_continuous(limits = c(-0.2, 5.2)) +
  scale_x_continuous(limits = c(-2, 5)) +
  scale_colour_manual(values = prettycols("Dark")[2:5]) +
  theme_void() +
  theme(legend.position = "none", 
        plot.margin = unit(c(0.5, 0, 0.5, 0), unit = "cm"))

# add legend and title
p <- cakes +
  geom_text(data = legend_data, 
            mapping = aes(x = x, y = y, colour = factor(stars), label = emoji("cake")),
            family = "EmojiOne",
            size = 20, 
            lineheight = 0.02) +
  geom_text(data = legend_data, 
            mapping = aes(x = x + 0.2, y = y + 0.1, colour = factor(stars), label = label),
            size = 8, 
            hjust = 0,
            lineheight = 0.02) +
  geom_text(data = plot_data, 
            mapping = aes(x = 3.5, y = 2.8, label = st),
            size = 12, 
            lineheight = 0.4) +
  labs(caption = "N. Rennie | Data: {bakeoff}") +
  theme(plot.caption = element_text(hjust = 0.5, 
                                    size = 30), 
        plot.background = element_rect(colour = "white", fill = "white"),
        panel.background = element_rect(colour = "white", fill = "white"))

# add logo
logo_file <- paste0(here::here(), "/2022/2022-10-25/logo.jpg")
ggdraw() + 
  draw_plot(p) +
  draw_image(
    logo_file, x = 0.98, y = 0.9, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.45
  )

# save gif
gg_playback(
  name = file.path("2022", "2022-10-25","20221025.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)
