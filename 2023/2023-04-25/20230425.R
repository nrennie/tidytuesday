library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(LondonMarathon)

# read in data
nicola_lm <- readr::read_csv("2023/2023-04-25/data/nicola_london_marathon.csv")

# start recording
gg_record(
  dir = file.path("2023", "2023-04-25", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "white"
highlight_col <- "#e00601"
dark_col <- "black"

# text
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = dark_col
)
cap <- "Since the first London Marathon in 1981, the number of people 
applying for a place in the race has drastically increased - especially
in recent years. Over 450, 000 people applied for the 2020 race, which
ended up taking place with only elite athletes due to Covid-19.<br><br>Data: Wikipedia via {LondonMarathon}"
st <- paste(cap, "<br><br>", social)

# background plot
p_bg <- ggplot() +
  geom_area(
    data = nicola_lm,
    mapping = aes(x = time, y = altitude),
    fill = highlight_col,
    alpha = 0.05
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# main plot
plot_data <- london_marathon |>
  select(Year, Applicants) |>
  drop_na()

p_main <- ggplot() +
  geom_segment(
    data = plot_data,
    mapping = aes(
      x = Year,
      xend = Year,
      y = 0,
      yend = Applicants
    )
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(
      x = Year, y = Applicants
    ),
    colour = highlight_col,
    size = 1.5,
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(
      x = Year,
      y = Applicants + 50000,
      label = paste0(Year, ":  ", format(Applicants, big.mark=","))
    ),
    colour = dark_col,
    size = 6,
    angle = 90,
    family = "Commissioner",
  ) +
  labs(
    title = "   London Marathon",
    tag = st
  ) +
  scale_x_continuous(limits = c(1980.5, 2020.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 550000), expand = c(0, 0)) +
  theme_void() +
  theme(
    plot.title = element_text(
      lineheight = 0.4,
      colour = dark_col,
      family = "Fraunces",
      hjust = 0,
      vjust = -8,
      face = "bold",
      size = 60,
      margin = margin(b = 5, t = 60, l = 15)
    ),
    plot.title.position = "plot",
    plot.tag.position = c(0.03, 0.5),
    plot.tag = element_textbox_simple(
      lineheight = 0.45,
      colour = dark_col,
      width = 0.9,
      family = "Commissioner",
      hjust = 0,
      halign = 0,
      size = 24,
      margin = margin(b = 10, t = 5)
    ),
    plot.margin = margin(-80, 0, 0, 0)
  )

# join together
p_bg + inset_element(p_main, 0, 0, 1, 1,
                     align_to = "full",
                     clip = TRUE)

# save gif
gg_playback(
  name = file.path("2023", "2023-04-25", "20230425.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
