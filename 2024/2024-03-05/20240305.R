# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(waffle)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-03-05")
trashwheel <- tuesdata$trashwheel


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
sysfonts::font_add(
  family = "Font Awesome 6",
  regular = "fonts/Font Awesome 6 Free-Solid-900.otf"
)
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
col_palette <- rcartocolor::carto_pal(length(unique(trashwheel$Name)) + 1, "Vivid")[1:length(unique(trashwheel$Name))]
names(col_palette) <- unique(trashwheel$Name)
highlight_col <- col_palette[1]

body_font <- "roboto"
title_font <- "roboto_slab"


# Data wrangling ----------------------------------------------------------

plot_data <- trashwheel |>
  group_by(Year, Name) |>
  summarise(HomesPowered = round(sum(HomesPowered) / 100)) |>
  ungroup() |>
  drop_na() |>
  filter(HomesPowered != 0) |> 
  mutate(Name = factor(
    Name, levels = c(
      "Mister Trash Wheel", "Professor Trash Wheel",
      "Captain Trash Wheel", "Gwynnda Trash Wheel"
    )
  )) |> 
  arrange(Name)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-03-05", "recording"),
  device = "png",
  width = 7,
  height = 5,
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
title <- "Over 45,000 homes powered by trash"
st <- glue("The Healthy Harbor initiative has four Trash Wheels collecting trash.
Trash Wheels are semi-autonomous trash interceptors that are placed at the end
of rivers, streams or other outfalls. Far too lazy to chase trash around the
ocean, Trash Wheels stay put and wait for the waste to flow to them. Sustainably
powered and built to withstand the biggest storms, Trash Wheels use a unique
blend of solar and hydro power to pull hundreds of tons of trash out of the
water each year. 
<span style='color: {col_palette['Mister Trash Wheel']};'>Mister Trash Wheel</span> 
was the first to start, and since then three more have joined the family: 
<span style='color: {col_palette['Professor Trash Wheel']};'>Professor Trash Wheel</span>, 
<span style='color: {col_palette['Captain Trash Wheel']};'>Captain Trash Wheel</span>, and
<span style='color: {col_palette['Gwynnda Trash Wheel']};'>Gwynnda Trash Wheel</span>.")
cap <- paste0(
  st,
  "<br><br>**Data**: Mr. Trash Wheel Baltimore Healthy Harbor<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_pictogram(
    mapping = aes(
      label = Name, colour = Name, values = HomesPowered
    ),
    flip = TRUE,
    n_rows = 5,
    size = 7,
    family = "Font Awesome 6"
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("home", "home", "home", "home")
  ) +
  scale_colour_manual(
    values = col_palette,
    breaks = c(
      "Mister Trash Wheel", "Professor Trash Wheel",
      "Captain Trash Wheel", "Gwynnda Trash Wheel"
    )
  ) +
  scale_x_discrete(
    expand = c(0, 0, 0, 0)
  ) +
  scale_y_continuous(
    labels = function(x) format(x * 5 * 100, big.mark = ","),
    expand = c(0, 0),
    breaks = c(0, 5, 10, 15, 20),
    limits = c(0, 20),
    minor_breaks = NULL
  ) +
  facet_wrap(~Year, nrow = 1, strip.position = "bottom") +
  labs(
    title = title,
    subtitle = cap
  ) +
  coord_fixed() +
  theme_minimal(
    base_family = body_font,
    base_size = 26
  ) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.margin = margin(5, 15, 5, 15),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.grid.major = element_line(
      linewidth = 0.4
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      face = "bold",
      size = rel(1.8),
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 20, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-03-05", paste0("20240305", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
