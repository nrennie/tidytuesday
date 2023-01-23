library(tidyverse)
library(showtext)
library(camcorder)
library(magick)
library(ggimage)

# load fonts
font_add_google("Cinzel Decorative", "cinzel")
font_add_google("Roboto", "roboto")
showtext_auto()

# load data
survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')

# data wrangling
plot_data <- survivalists |> 
  filter(result == 1) |> 
  select(season, name, days_lasted) |> 
  mutate(name = recode(name, "Jim Baird" = "Jim Baird / Ted Baird")) |> 
  filter(name != "Ted Baird") |> 
  mutate(season = factor(season)) 

# start recording
gg_record(
  dir = file.path("2023", "2023-01-24", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 4, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# subtitle
st <- "Alone is an American survival series which follows the struggles of 10 individuals as they survive alone in the wilderness for as long as possible using a limited amount of survival equipment. Season 7 winner, Roland Welker, holds the record after surviving for 100 days."

# Process images
# custom function to apply border to circle image with magick
# Code: https://github.com/tashapiro/tanya-data-viz/blob/main/spotify-artists/scripts/generate-image-labels.R
border <- function(im) {
  ii <- magick::image_info(im)
  ii_min <- min(ii$width, ii$height)
  
  img <- magick::image_blank(width = ii_min, height = ii_min, color = "none")
  drawing <- image_draw(img)
  symbols(ii_min/2, ii_min/2, circles = ii_min/2, bg = 'white', inches = FALSE, add = TRUE)
  dev.off()
  
  x = image_composite(image_scale(drawing, "x430"), image_scale(im, "x400"), offset = "+15+15")
  
  x
}

# Images: https://www.distractify.com/p/alone-winners-where-are-they-now
# Image 6: https://www.instagram.com/p/CYpAMGxpok7
# Image 7: https://www.instagram.com/p/CcM04VrLQyA
# Image 8: https://www.instagram.com/p/CgkNf3LpPMx
# plot
ggplot(data = plot_data) +
  geom_segment(mapping = aes(x = 0, 
                             xend = days_lasted,
                             y = season,
                             yend= season),
               colour = "#f4f7f7",
               linewidth = 1) +
  geom_text(mapping = aes(x = days_lasted + 12, 
                          y = season,
                          label = name),
            size = 9,
            hjust = 0,
            family = "roboto",
            colour = "#f4f7f7") +
  geom_image(mapping = aes(x = days_lasted,
                           y = season,
                           image = paste0("2023/2023-01-24/images/", season, ".png")),
             asp = 4.5/6,
             size = 0.12,
             image_fun = border) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                     limits = c(-5, 150),
                     expand = c(0, 0)) +
  labs(title = "ALONE",
       subtitle = str_wrap(st, 70),
       x = "Days survived",
       y = "Season") +
  theme(axis.text = element_text(family = "roboto",
                                 size = 24,
                                 lineheight = 0.4,
                                 colour = "#f4f7f7"),
        axis.title = element_text(family = "roboto",
                                  size = 24,
                                  lineheight = 0.4,
                                  margin = margin(t = 10),
                                  colour = "#f4f7f7"),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "roboto",
                                     size = 24,
                                     hjust = 0.5,
                                     lineheight = 0.4,
                                     margin = margin(b = 10),
                                     colour = "#f4f7f7"),
        plot.title = element_text(family = "cinzel",
                                  size = 44,
                                  hjust = 0.5,
                                  margin = margin(b = 10),
                                  colour = "#f4f7f7"),
        plot.margin = margin(10, 10, 10, 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = alpha("#f4f7f7", 0.3)),
        plot.background = element_rect(fill = "#203d58", colour = "#203d58"),
        panel.background = element_rect(fill = "#203d58", colour = "#203d58"))

# save gif
gg_playback(
  name = file.path("2023", "2023-01-24","20230124.gif"),
  first_image_duration = 4,
  width = 500, 
  height = 600,
  units = "px",
  last_image_duration = 20,
  frame_duration = .25,
  background = "#203d58"
)




