# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-21")
rladies_chapters <- tuesdata$rladies_chapters


# Load fonts --------------------------------------------------------------

font_add_google("Lato", "lato")
font_add_google("Yanone Kaffeesatz", "yanone")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey98"
text_col <- "grey5"
highlight_col <- "#562457"

body_font <- "lato"
title_font <- "yanone"


# Data wrangling ----------------------------------------------------------

plot_data <- rladies_chapters |>
  select(date, location, year) |>
  mutate(date = lubridate::wday(date, label = TRUE, week_start = 1)) |>
  count(date, location, year) |>
  mutate(location = case_when(
    location == "inperson" ~ "In person",
    location == "online" ~ "Online"
  ))

most_common <- plot_data |> 
  group_by(date, location) |> 
  summarise(n = sum(n)) |> 
  arrange(location, desc(n))

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-11-21", "recording"),
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
title <- "R-Ladies Chapter Events"
st <- "For both in person and online events, Thursdays are the most common day 
of the week to hold R-Ladies chapter events."
cap <- paste0(
  "**Data**: Meetup via Fgazzelloni<br>", social
)


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_point(
    mapping = aes(x = date, y = year, size = n),
    colour = bg_col,
    fill = bg_col,
    pch = 21
  ) +
  geom_point(
    mapping = aes(x = date, y = year, size = n),
    colour = highlight_col,
    fill = alpha(highlight_col, 0.5),
    pch = 21
  ) +
  facet_wrap(~location) +
  scale_y_reverse(breaks = min(plot_data$year):max(plot_data$year)) +
  guides(size = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    title = "Number of events")
    ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_minimal(base_size = 24, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 15),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = highlight_col,
      linewidth = 0.3
    ),
    strip.text = element_text(
      family = title_font,
      size = rel(1.2)
    ),
    axis.title = element_blank(),
    plot.title = element_textbox_simple(
      colour = text_col,
      size = 50,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.justification = c(0.44, 0.5),
    legend.position = "bottom",
    legend.title = element_text(
      margin = margin(t = -20)
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-11-21", paste0("20231121", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
