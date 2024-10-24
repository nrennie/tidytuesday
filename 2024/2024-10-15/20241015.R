
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-10-15")
orcas <- tuesdata$orcas


# Load fonts --------------------------------------------------------------

font_add_google("Delius Swash Caps", "delius")
showtext_auto()

title_font <- "delius"
body_font <- "delius"


# Define colours ----------------------------------------------------------

# V1
col_palette <- c("#71B48D", "#404E7C", "#3a3559", "#260F26")
na_col <- "#D4E8DC"
bg_col <- "#F8F1FF"
text_col <- "#260F26"
highlight_col <- "#71B48D"

# V2

col_palette <- c("grey70", "grey80", "grey90", "white")
na_col <- "black"
bg_col <- "black"
text_col <- "white"
highlight_col <- "white"


# Data wrangling ----------------------------------------------------------

start_date <- ymd("20180101")
end_date <- ymd("20231231")
plot_data <- orcas |> 
  select(date) |> 
  count(date) |> 
  filter(date >= start_date & date <= end_date) |> 
  complete(date = seq.Date(start_date, end_date, by = "days"), 
           fill = list(n = NA)) |> 
  mutate(year = year(date),
         doy = yday(date)) |> 
  select(-date) |> 
  mutate(y = 1)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-10-15", "recording"),
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  twitter = NA
)
cap <- paste0(
  "**Data**: Center for Whale Research<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

g <- ggplot(
  data = plot_data,
  mapping = aes(
    x = doy,
    y = factor(year),
    fill = n
  )
) +
  scale_fill_gradientn(
    colours = col_palette,
    na.value = na_col
  ) +
  labs(caption = cap) +
  theme_void(base_family = body_font, base_size = 24)


# Polar version
g + 
  geom_col(
    colour = NA
  ) +
  coord_polar() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 1
    )
  )


# Square version
g + 
  geom_col(
    colour = NA,
    width = 1
  ) +
  scale_y_discrete(limits = rev) +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 3, t = -18, l = 3),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 1
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-10-15", paste0("20241015", ".png")),
  bg = bg_col,
  width = 6,
  height = 6
)

gg_playback(
  name = file.path("2024", "2024-10-15", paste0("20241015", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
