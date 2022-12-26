library(tidyverse)
library(camcorder)
library(showtext)

# load fonts
font_add_google("Zen Dots", "zen")
font_add_google("Racing Sans One", "racing")
showtext_auto()

# load data
tlBooks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlBooks.csv')
tlFootnotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlFootnotes.csv')

# prep data
plot_data <- tlBooks |> 
  filter(series == "TNG") |> 
  select(title, number, anthology) |> 
  filter(anthology == "YA") |> 
  distinct() |> 
  arrange(number) |> 
  mutate(title = str_remove(title, "Starfleet Academy: "),
         title = toupper(title)) |> 
  select(-anthology) |> 
  mutate(l_mm = 25.4 * strwidth(title, family = "racing", units = "inches"),
         s = 16 * max(l_mm)/l_mm,
         h = s / 10) |> 
  mutate(y = cumsum(lag(h/2, default = 0) + h/2))

# start recording
gg_record(
  dir = file.path("2022", "2022-12-27", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 8.3, # width of saved image
  height = 11.7, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# stars
set.seed(123)
stars <- data.frame(x = runif(250, -0.5, 0.5),
                    y = runif(250, min(plot_data$y)-2, max(plot_data$y)+5))
# plot
ggplot(plot_data) +
  geom_point(data = stars,
             aes(x = x, y = y),
             colour = "white",
             size = 0.1) +
  geom_text(aes(x = 0, y = y, label = title, size = s),
            fontface = "bold", family = "racing", colour = "#f6fa55") +
  scale_y_reverse() +
  scale_size_identity() +
  labs(title = toupper("Star Trek: The Next Generation"),
       subtitle = toupper("Starfleet Academy"),
       caption = "N. Rennie | Data: {rtrek}") +
  theme_void() +
  theme(plot.title = element_text(family = "zen", size = 60, lineheight = 0.1,
                                  hjust = 0.5, margin = margin(b = 10),
                                  colour = "#eb3507", face = "bold"),
        plot.subtitle = element_text(family = "zen", size = 40, lineheight = 0.4,
                                     hjust = 0.5, margin = margin(t = 10, b = 10),
                                     colour = "#fdfdf7"),
        plot.caption = element_text(family = "zen", size = 40, lineheight = 0.4,
                                     hjust = 0.5, margin = margin(t = 10, b = 20),
                                     colour = "#fdfdf7"),
        plot.margin = margin(40, 20, 20, 20),
        plot.background = element_rect(colour = "black", fill = "black"),
        panel.background = element_rect(colour = "black", fill = "black"))

# save gif
gg_playback(
  name = file.path("2022", "2022-12-27","20221227.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)


