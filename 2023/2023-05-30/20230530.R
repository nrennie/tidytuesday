library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)

# load fonts
font_add_google("Poppins", "Poppins")
font_add_google("Cinzel Decorative", "Cinzel")
showtext_auto()

# read in data
centenarians <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv")

# make events data
events <- data.frame(
  x = ymd(
    "18800901", "19031217", "19120412", "19440421",
    "19580101", "19770910"
  ),
  y = c(1, 1.5, 0.7, 1.0, 0.4, 1.5),
  label = c(
    "Women in France allowed to attend university",
    "First flight",
    "Sinking of the Titanic",
    "Women in France allowed to vote",
    "Founding of the EU",
    "Last execution by guillotine in France"
  )
)

# start recording
gg_record(
  dir = file.path("2023", "2023-05-30", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 12, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "#fafafa"
dark_col <- "#555656"
highlight_col <- nrBrand::nr_mid

# text
title <- "Jeanne Calment"
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = dark_col,
  font_family = "Poppins"
)
st <- "Jeanne Calment was the oldest person ever to have lived
(whose age has been independently verified). She was born in
France in 1875, and lived to the age of 122 years and 164 days.
Jeanne outlived her only child by over sixty years, and her only
grandchild by over thirty years."
cap <- paste0("**Data**: Wikipedia<br>", social)

# plot
g <- ggplot() +
  geom_segment(
    data = data.frame(
      x = ymd("18750101"),
      xend = ymd("19971231"),
      y = 0,
      yend = 0
    ),
    mapping = aes(
      x = x, xend = xend, y = y, yend = yend
    ),
    colour = dark_col,
    linewidth = 0.8
  ) 

# styling and theme
g2 <- g +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = "",
    y = ""
  ) +
  scale_x_date(
    breaks = ymd(paste0(seq(1875, 2000, 25), "0101")),
    minor_breaks = ymd(paste0(seq(1855, 2020, 5), "0101")),
    limits = ymd(c("18550101", "20200101")),
    labels = scales::label_date(format = "%Y")
  ) +
  scale_y_continuous(
    limits = c(0, 2.5)
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(
    base_size = 40,
    base_family = "Poppins"
  ) +
  theme(
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.background = element_rect(
      fill = alpha(dark_col, 0.1),
      colour = dark_col,
      linewidth = 0.3
    ),
    plot.title = element_textbox_simple(
      family = "Cinzel",
      face = "bold",
      margin = margin(b = 10),
      colour = dark_col,
      size = 60,
      hjust = 0,
      halign = 0,
      lineheight = 0.4,
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.5,
      colour = dark_col,
      margin = margin(b = 15),
      hjust = -0.5,
      halign = 0
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.55,
      margin = margin(t = 0),
      colour = dark_col,
      hjust = 0,
      halign = 0
    ),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.5,
      colour = dark_col
    ),
    panel.grid.minor.x = element_line(
      linewidth = 0.1,
      colour = dark_col
    ),
    axis.line = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(20, 35, 10, 5),
  )

# Add event periods
g3 <- g2 +
  geom_rect(
    data = data.frame(
      xmin = ymd("19140728"),
      xmax = ymd("19181111")
    ),
    mapping = aes(
      xmin = xmin, xmax = xmax, ymin = 0, ymax = 2.5
    ),
    alpha = 0.3,
    fill = highlight_col
  ) +
  geom_rect(
    data = data.frame(
      xmin = ymd("19390901"),
      xmax = ymd("19450902")
    ),
    mapping = aes(
      xmin = xmin, xmax = xmax, ymin = 0, ymax = 2.5
    ),
    alpha = 0.3,
    fill = highlight_col
  ) +
  geom_rect(
    data = data.frame(
      xmin = ymd("19180201"),
      xmax = ymd("19200430")
    ),
    mapping = aes(
      xmin = xmin, xmax = xmax, ymin = 0, ymax = 2.5
    ),
    alpha = 0.4,
    fill = highlight_col
  ) +
  geom_textbox(
    data = data.frame(
      x = ymd(c("19160901", "19190301", "19420901")),
      y = c(2.1, 1.8, 1.9),
      label = c("World War I", "Spanish Flu", "World War II")
    ),
    mapping = aes(
      x = x,
      y = y,
      label = label
    ),
    halign = 0.5, hjust = 0.5,
    vjust = 0,
    size = 14,
    lineheight = 0.5,
    box.color = dark_col,
    fill = alpha(bg_col, 0.8),
    colour = dark_col,
    maxheight = unit(8, "lines"),
    minwidth = unit(4, "lines"),
    maxwidth = unit(7, "lines")
  )

# add events
g4 <- g3 +
  geom_point(
    data = events,
    mapping = aes(
      x = x, y = 0
    ),
    colour = dark_col,
    size = 3
  ) +
  geom_segment(
    data = events,
    mapping = aes(
      x = x, xend = x, y = 0, yend = y
    ),
    linewidth = 1
  ) +
  geom_textbox(
  data = events,
  mapping = aes(
    x = x,
    y = y,
    label = label
  ),
  halign = 0.5, hjust = 0.5,
  vjust = 0,
  size = 14,
  lineheight = 0.5,
  box.color = dark_col,
  fill = alpha(bg_col, 0.9),
  colour = dark_col,
  maxheight = unit(8, "lines"),
  minwidth = unit(4, "lines"),
  maxwidth = unit(7, "lines")
)

# add Jeanne Calment labels
g4 +
  geom_segment(
    data = data.frame(
      x = as.Date(unlist(filter(centenarians, name == title)[,c("birth_date", "death_date")])),
      xend = as.Date(unlist(filter(centenarians, name == title)[,c("birth_date", "death_date")])),
      y = 0.2
    ),
    mapping = aes(
      x = x, xend = x, y = 0, yend = y
    ),
    linewidth = 1
  ) +
  geom_textbox(
    data = filter(centenarians, name == title),
    mapping = aes(
      x = birth_date, y = 0.2,
      label = "Jeanne Calment was born in Provence,
      France on February 21, 1875."
    ),
    halign = 0.5, hjust = 0.5,
    vjust = 0,
    size = 14,
    lineheight = 0.5,
    box.color = dark_col,
    fill = alpha(highlight_col, 0.95),
    colour = bg_col,
    maxheight = unit(8, "lines"),
    minwidth = unit(12, "lines"),
    maxwidth = unit(16, "lines")
  ) +
  geom_textbox(
    data = filter(centenarians, name == title),
    mapping = aes(
      x = death_date, y = 0.2,
      label = "Jeanne Calment died on August 4, 1997 at
      the age of 122."
    ),
    halign = 0.5, hjust = 0.5,
    vjust = 0,
    size = 14,
    lineheight = 0.5,
    box.color = dark_col,
    fill = alpha(highlight_col, 0.95),
    colour = bg_col,
    maxheight = unit(8, "lines"),
    minwidth = unit(10, "lines"),
    maxwidth = unit(16, "lines")
  ) +
  geom_point(
    data = filter(centenarians, name == title),
    mapping = aes(
      x = birth_date,
      y = 0
    ),
    pch = 21,
    fill = highlight_col,
    colour = dark_col,
    size = 5
  ) +
  geom_point(
    data = filter(centenarians, name == title),
    mapping = aes(
      x = death_date,
      y = 0
    ),
    pch = 21,
    fill = highlight_col,
    colour = dark_col,
    size = 5
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-05-30", "20230530.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
