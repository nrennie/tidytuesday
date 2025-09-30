# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-09-30")
cranes <- tuesdata$cranes


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

highlight_col <- "#bd0026"
text_col <- "black"
bg_col <- "white"

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------

plot_data <- cranes |>
  mutate(
    year = year(date),
    day = yday(date),
    new_date = dmy(paste0(day(date), "/", month(date), "/", 2024))
  ) |>
  filter(
    new_date <= dmy("01/07/2024")
  )

year_data <- plot_data |>
  group_by(year) |>
  summarise(min_d = min(new_date), max_d = max(new_date)) |>
  ungroup()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-09-30", "recording"),
  device = "png",
  width = 7,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Spring is arriving earlier at Lake Hornborgasjön"
st <- "For more than 30 years, cranes stopping at Lake Hornborgasjön in Västergötland, Sweden have been counted from the Hornborgasjön field station in the spring and the fall as they pass by during their yearly migration. When there are the most cranes depends on whether spring is early or late. It also depends on when the winds from the south are suitable for crane flight."
cap <- paste0(
  "**Data**: Hornborgasjön Field Station<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_segment(
    data = data.frame(
      x = seq(1995, 2020, 5),
      ymin = ymd("20240508"),
      ymax = ymd("20240415")
    ),
    mapping = aes(x = x, y = ymin, yend = ymax),
    linetype = "dashed",
    colour = text_col,
    linewidth = 0.5,
    alpha = 0.3
  ) +
  geom_tile(
    data = plot_data,
    mapping = aes(x = year, y = new_date, fill = observations),
    width = 0.8
  ) +
  geom_rect(
    data = year_data,
    mapping = aes(
      xmin = year - 0.4, xmax = year + 0.4,
      ymin = min_d, ymax = max_d
    ),
    colour = alpha(text_col, 0.7),
    fill = "transparent",
    linewidth = 0.5
  ) +
  geom_textbox(
    data = data.frame(
      x = 2014,
      y = ymd("20240304"),
      label = "**Black** areas indicate where counts were cancelled due to bad weather."
    ),
    mapping = aes(x = x, y = y, label = label),
    fill = "transparent",
    colour = text_col,
    box.colour = "transparent",
    maxwidth = unit(2, "inch")
  ) +
  annotate("curve", x = 2009, xend = 2007.5, 
           y = ymd("20240306"), yend = ymd("20240308"),
           arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
           curvature = -0.1,
           colour = text_col) +
  geom_textbox(
    data = data.frame(
      x = 1998,
      y = ymd("20240310"),
      label = "**Yellow** (lighter) areas indicate where fewer cranes were observed."
    ),
    mapping = aes(x = x, y = y, label = label),
    fill = "transparent",
    colour = text_col,
    box.colour = "transparent",
    maxwidth = unit(2, "inch")
  ) +
  annotate("curve", x = 1996, xend = 1998, 
           y = ymd("20240315"), yend = ymd("20240321"),
           arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
           curvature = -0.1,
           colour = text_col) +
  geom_textbox(
    data = data.frame(
      x = 2023,
      y = ymd("20240504"),
      label = glue("<span style='color:{highlight_col}'>**Red**</span> (darker) areas indicate where more cranes were observed.")
    ),
    mapping = aes(x = x, y = y, label = label),
    fill = "transparent",
    colour = text_col,
    box.colour = "transparent",
    maxwidth = unit(1, "inch")
  ) +
  annotate("curve", x = 2020.5, xend = 2019, 
           y = ymd("20240501"), yend = ymd("20240420"),
           arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
           curvature = -0.1,
           colour = text_col) +
  scale_x_continuous(
    limits = c(1993, 2025),
    breaks = seq(1995, 2024, 5)
  ) +
  scale_y_continuous(
    trans = c("date", "reverse"),
    limits = ymd(c(
      "20240508", "20240301"
    )),
    breaks = ymd(c(
      "20240301", "20240401", "20240501"
    )),
    labels = c("March", "April", "May")
  ) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "black") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    x = NULL, y = NULL,
    title = title, subtitle = st,
    caption = cap
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 30, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(hjust = 1, margin = margin(r = -5)),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-09-30", paste0("20250930", ".png")),
  width = 7,
  height = 6,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-09-30", paste0("20250930", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
