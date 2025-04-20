# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-04-22")
daily_accidents <- tuesdata$daily_accidents
daily_accidents_420 <- tuesdata$daily_accidents_420


# Load fonts --------------------------------------------------------------

font_add_google("Poppins", "poppins")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#E9DCF9"
text_col <- "#260B47"
highlight_col <- "#55199F"

body_font <- "poppins"
title_font <- "poppins"


# Data wrangling ----------------------------------------------------------

plot_data <- daily_accidents |>
  arrange(date) |>
  mutate(
    year = year(date),
    dofy = yday(date)
  ) |>
  select(dofy, year, fatalities_count) |>
  uncount(fatalities_count, .remove = FALSE)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-04-22", "recording"),
  device = "png",
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Fatal Car Crashes in the United States"
st <- "Each point represents a fatal car crash, and darker coloured points are crashes that occurred on days with a higher number of fatalities."
cap <- paste0(
  "**Data**: osf.io/qnrg6<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_jitter(
    data = plot_data,
    mapping = aes(x = dofy, y = year, colour = fatalities_count),
    size = 0.0001,
    height = 0.5,
    width = 0.2
  ) +
  scale_x_continuous(
    breaks = c(1, 91, 182, 274),
    labels = c("Jan", "Apr", "Jul", "Oct")
  ) +
  scale_y_reverse() +
  scale_colour_gradient(
    low = "#D3B9F3",
    high = text_col
  ) +
  labs(title = title, 
       subtitle = st,
       caption = cap,
       y = "", x = "") +
  coord_cartesian(
    expand = FALSE
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    legend.position = "none",
    text = element_text(colour = text_col),
    panel.grid = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.x = element_text(
      colour = text_col, hjust = 0
    ),
    axis.text.y = element_text(
      colour = text_col, margin = margin(l = -10)
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-04-22", paste0("20250422", ".png")),
  height = 10,
  width = 10,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-04-22", paste0("20250422", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
