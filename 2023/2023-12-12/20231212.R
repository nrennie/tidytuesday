# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-12-12")
holiday_movies <- tuesdata$holiday_movies
holiday_movie_genres <- tuesdata$holiday_movie_genres


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Mountains of Christmas", "xmas")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#ffffff"
text_col <- "#000000"
highlight_col <- "#D0312D"

body_font <- "roboto"
title_font <- "xmas"


# Data wrangling ----------------------------------------------------------

# average runtime per year
plot_data <- holiday_movies |>
  filter(christmas) |>
  filter(genres %in% c("Comedy", "Horror", "Romance", "Animation", "Drama")) |>
  mutate(decade = 10*floor(year / 10)) |> 
  group_by(genres, decade) |>
  summarise(avg_time = mean(runtime_minutes, na.rm = TRUE)) |> 
  ungroup() |> 
  drop_na()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-12-12", "recording"),
  device = "png",
  width = 8,
  height = 3.5,
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
title <- "Christmas Movies"
st <- glue::glue("The runtime of Christmas films listed on IMDb varies based on 
                 genre, with Christmas drama movies decreasing in runtime over 
                 time since 1960. In contrast, Christmas horror movies have seen 
                 an increase in average runtime, especially in the last 10 years.")
cap <- paste0(
  "**Data**: IMDb<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

# plot area charts
inset_plot <- ggplot() +
  geom_area(
    data = plot_data,
    mapping = aes(x = decade, y = avg_time),
    fill = highlight_col,
    colour = highlight_col
  ) +
  scale_x_continuous(breaks = c(1960, 2010)) +
  facet_wrap(~genres, nrow = 1) +
  theme_void(base_size = 30, base_family = body_font) +
  theme(strip.text = element_text(margin = margin(t = 12),
                                  family = title_font,
                                  size = 32),
        axis.text.x = element_text(margin = margin(t = 0),
                                   size = 20))
inset_plot

# plot background film strip
main_plot <- ggplot() +
  annotate(
    geom = "rect",
    xmin = 0,
    xmax = 5,
    ymin = 1,
    ymax = 2,
    fill = text_col,
    colour = text_col
  ) +
  # white boxes at top and bottom
  geom_rect(
    data = data.frame(
      xmin = seq(0, 5, by = 0.1), xmax = seq(0.07, 5.1, by = 0.1),
      ymin = rep(1.96, times = length(seq(0, 5, by = 0.1))),
      ymax = rep(1.9, times = length(seq(0, 5, by = 0.1)))
    ),
    mapping = aes(
      xmin = xmin, ymin = ymin,
      xmax = xmax, ymax = ymax
    ),
    fill = bg_col,
    colour = bg_col
  ) +
  geom_rect(
    data = data.frame(
      xmin = seq(0, 5, by = 0.1), xmax = seq(0.07, 5.1, by = 0.1),
      ymin = rep(1.04, times = length(seq(0, 5, by = 0.1))),
      ymax = rep(1.10, times = length(seq(0, 5, by = 0.1)))
    ),
    mapping = aes(
      xmin = xmin, ymin = ymin,
      xmax = xmax, ymax = ymax
    ),
    fill = bg_col,
    colour = bg_col
  ) +
  # main white boxes
  geom_rect(
    data = data.frame(
      xmin = seq(0.06, 5, by = 1), xmax = seq(0.94, 5.1, by = 1),
      ymin = rep(1.17, times = 5),
      ymax = rep(1.83, times = 5)
    ),
    mapping = aes(
      xmin = xmin, ymin = ymin,
      xmax = xmax, ymax = ymax
    ),
    fill = bg_col,
    colour = bg_col
  ) +
  # styling
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(1.0, 2.05)) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 30, base_family = body_font) +
  theme(
    plot.margin = margin(5, 0, 5, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      face = "bold",
      size = 65,
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )
main_plot

# join
main_plot + inset_element(inset_plot,
                          0.01, 0.18, 0.99, 0.85, 
                          align_to = "plot") &
  theme(plot.margin = margin(0, 0, 0, 0))


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-12-12", paste0("20231212", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
