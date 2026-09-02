# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(sf)
library(ggiraph)
library(emojifont)
library(ggmapcn)
library(htmltools)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-09-01")
world_castles <- tuesdata$world_castles

# https://geoportal.statistics.gov.uk/datasets/d4f6b6bdf58a45b093c0c189bdf92e9d_0/explore?filters=eyJDVFJZMjRDRCI6WyJTOTIwMDAwMDMiXX0%3D
scotland <- read_sf("../30DayMapChallenge/2025/data/Countries_December_2024_Boundaries_UK_BFC_-6467230212120045634/CTRY_DEC_2024_UK_BFC.shp")


# Load fonts --------------------------------------------------------------

font_add_google("Felipa")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Felipa"
body_font <- "Felipa"


# Define colours and fonts-------------------------------------------------

bg_col <- "#f9d5a1"
text_col <- "#563410"


# Data wrangling ----------------------------------------------------------

plot_data <- world_castles |>
  filter(iso == "GB-SCT") |>
  select(name, category, lat, lon, year, image) |>
  mutate(
    year = if_else(is.na(year), "unavailable", as.character(year)),
    text = fontawesome("fa-fort-awesome")
  ) |> 
  rowwise() |> 
  mutate(
    label = HTML(glue("<b>{name}</b><br>Date: {year}<br><img src='{image}' width='200' alt='Picture of castle'><br>"))
  )

plot_sf <- plot_data |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  ) |>
  st_transform(
    crs = st_crs(scotland)
  )


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Castles of Scotland"
st <- glue("There are {nrow(plot_data)} castles in Scotland listed in the Castlemap database, though it's estimated there are around 3,000 in total.")
cap <- source_caption(source = "Castlemap", graphic = social, sep = " | ")


# Plot --------------------------------------------------------------------

g_int <- ggplot() +
  geom_sf(
    data = scotland,
    fill = "#fdeaca",
    colour = text_col,
    linewidth = 0.1
  ) +
  geom_sf(
    data = plot_sf,
    mapping = aes(shape = category),
    colour = text_col,
    size = 3
  ) +
  geom_sf_text(
    data = plot_sf,
    mapping = aes(label = text),
    colour = bg_col,
    family = "fontawesome-webfont",
    size = 1.5
  ) +
  geom_sf_interactive(
    data = plot_sf,
    mapping = aes(shape = category, data_id = name, tooltip = label),
    colour = "transparent",
    size = 3
  ) +
  annotation_compass(
    which_north = "true",
    location = "br",
    style = compass_rose_simple(
      fill = c("#fdeaca", text_col),
      line_col = text_col,
      text_col = text_col,
      text_family = body_font
    )
  ) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 12, base_family = body_font) +
  theme(
    plot.margin = margin(0, 0, 10, 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = text_col, linewidth = 1),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -50, t = 40, l = 10),
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -65, t = 55, l = 10),
      size = rel(0.9),
      maxwidth = 0.8,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = -15, l = 10),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.12, 0.75),
    legend.key = element_rect(colour = bg_col, fill = "transparent"),
    legend.text = element_text(size = rel(1.2),
                               colour = text_col),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = text_col,
      linetype = "dashed",
      linewidth = 0.2
    )
  )

g_int +
  canvas(
    width = 5, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-09-01", paste0("20260901", ".png"))
)


# Interactive -------------------------------------------------------------

girafe(
  ggobj = g_int,
  bg = bg_col,
  width_svg = 5,
  height_svg = 7,
  options = list(
    opts_tooltip(
      delay_mouseover = 500,
      opacity = 0.9,
      css = glue("
        padding: 5pt;
        font-family: {body_font};
        font-size: 2rem;
        background-color: {bg_col};
        color: {text_col};
        border: solid;
        border-color: {text_col};
        border-radius: 5px;
        border-width: 2px")
    ),
    opts_hover(css = "opacity: 1;"),
    opts_hover_inv(css = "opacity: 0.7;"),
    opts_toolbar(hidden = c("saveaspng", "fullscreen")),
    opts_zoom(max = 1)
  )
)
