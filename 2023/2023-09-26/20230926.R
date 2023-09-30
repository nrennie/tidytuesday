# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggwordcloud)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-09-26")
richmondway <- tuesdata$richmondway


# Load fonts --------------------------------------------------------------

font_add_google("Archivo Black", "archivo")
showtext_auto()


# Define colours ----------------------------------------------------------

tl_red <- "#cb5d51"
tl_blue <- "#0058b5"
bg_col <- "#002C5A"
text_col <- "white"

# Data wrangling ----------------------------------------------------------

plot_data <- richmondway |>
  select(F_count_RK, Imdb_rating) |>
  mutate(word = "F**K")

avg_rating <- mean(richmondway$Imdb_rating)

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-09-26", "recording"),
  device = "png",
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = tl_red,
  font_colour = text_col,
  font_family = "archivo"
)
st <- "The number of f\\*\\*ks expressed by Roy Kent in Ted Lasso. Each 'F\\*\\*K' is 
an episode with size showing number of F\\*\\*Ks. Red indicates above average IMDb 
rating, and blue below.<br>"
cap <- paste0(
  st,
  "<br>", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  aes(
    label = word, size = F_count_RK,
    color = Imdb_rating
  )
) +
  geom_text_wordcloud_area(
    eccentricity = 1.1,
    seed = 123,
    tstep = 0.04,
    family = "archivo"
  ) +
  scale_size_area(
    max_size = 41
  ) +
  scale_colour_gradient2(
    low = tl_red, high = tl_blue, midpoint = avg_rating
  ) +
  coord_cartesian(
    expand = FALSE
  ) +
  labs(
    caption = cap
  ) +
  theme_void(base_size = 22, base_family = "archivo") +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 0, r = 10, l = 10),
      lineheight = 0.5,
      family = "archivo"
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-09-26", paste0("20230926", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
