# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(geofacet)
library(cowplot)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-04-09")
eclipse_total_2024 <- tuesdata$eclipse_total_2024


# Load fonts --------------------------------------------------------------

sysfonts::font_add_google("Roboto", "roboto")
sysfonts::font_add_google("Carter One", "carter")
showtext::showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#FEF5E1"
text_col <- "black"
col_palette <- rcartocolor::carto_pal(n = 4, name = "SunsetDark")
highlight_col <- col_palette[3]

body_font <- "roboto"
title_font <- "carter"


# Data wrangling ----------------------------------------------------------

plot_data <- eclipse_total_2024 |>
  select(-c(name, lat, lon)) |>
  group_by(state) |>
  summarise(
    across(everything(), ~ hms::hms(lubridate::seconds_to_period(mean(.x))))
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-04-09", "recording"),
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
title <- "2024 US Solar Eclipse"
st <- "On April 8, 2024, a total solar eclipse crossed North and Central
America creating a path of totality. During a total solar eclipse, the
Moon completely blocks the Sun while it passes between the Sun and Earth.
The sky darkens as if it were dawn or dusk and those standing in the
path of totality may see the Sun’s outer atmosphere (the corona) if weather
permits."
cap <- paste0(
  "**Data**: NASA's Scientific Visualization Studio<br>**Graphic**:", social
)


# Define grid -------------------------------------------------------------

us_grid <- geofacet::us_state_grid2 |>
  as_tibble() |>
  dplyr::filter(code %in% plot_data$state)


# Plot --------------------------------------------------------------------

p_main <- ggplot(data = plot_data) +
  geom_rect(
    mapping = aes(
      xmin = hms::hms(hours = 17),
      xmax = eclipse_1,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[1]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_1,
      xmax = eclipse_2,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[2]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_2,
      xmax = eclipse_3,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[3]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_3,
      xmax = eclipse_4,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[4]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_4,
      xmax = eclipse_5,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[3]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_5,
      xmax = eclipse_6,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[2]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_6,
      xmax = hms::hms(hours = 21),
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[1]
  ) +
  geom_text(
    mapping = aes(
      x = hms::hms(hours = 19),
      y = 0.5,
      label = state
    ),
    family = title_font,
    colour = alpha(bg_col, 0.6),
    size = 14
  ) +
  labs(
    title = title,
    tag = st,
    caption = cap
  ) +
  geofacet::facet_geo(~state, grid = us_grid) +
  scale_x_time(
    breaks = hms::hms(hours = c(18, 20)),
    labels = scales::label_time(format = "%H:%M")
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 24, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, -100),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 5, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      maxwidth = 0.55,
      family = body_font
    ),
    plot.tag.position = c(0.19, 0.73),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.25,
      halign = 0.25,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      size = rel(2),
      family = title_font
    ),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0.1, "lines"),
  )
record_polaroid()



# Legend plot -------------------------------------------------------------

p_inset <- ggplot(data = filter(plot_data, state == "AR")) +
  geom_rect(
    mapping = aes(
      xmin = hms::hms(hours = 17),
      xmax = eclipse_1,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[1]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_1,
      xmax = eclipse_2,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[2]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_2,
      xmax = eclipse_3,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[3]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_3,
      xmax = eclipse_4,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[4]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_4,
      xmax = eclipse_5,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[3]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_5,
      xmax = eclipse_6,
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[2]
  ) +
  geom_rect(
    mapping = aes(
      xmin = eclipse_6,
      xmax = hms::hms(hours = 21),
      ymin = 0,
      ymax = 1
    ),
    fill = col_palette[1]
  ) +
  geom_text(
    mapping = aes(
      x = hms::hms(hours = 19),
      y = 0.5,
      label = state
    ),
    family = title_font,
    colour = alpha(bg_col, 0.6),
    size = 14
  ) +
  geom_text(
    data = data.frame(
      x = c(filter(plot_data, state == "AR")$eclipse_1,
            filter(plot_data, state == "AR")$eclipse_2,
            mean(
              c(filter(plot_data, state == "AR")$eclipse_3,
                filter(plot_data, state == "AR")$eclipse_4)
            ),
            filter(plot_data, state == "AR")$eclipse_5,
            filter(plot_data, state == "AR")$eclipse_6),
      y = c(0.8, 0.6, 0.2, 0.6, 0.8),
      label = c("Avg. time moon first contacts sun",
                "Avg. time eclipse is at 50%",
                "Avg. time of totality",
                "Avg. time eclipse is back at 50%",
                "Avg. time moon last contacts sun")
    ),
    mapping = aes(
      x = x, y = y, label = str_wrap(label, 10)
    ),
    family = body_font,
    size = 6,
    lineheight = 0.5
  ) +
  scale_x_time(
    breaks = hms::hms(hours = c(17.5, 20.5)),
    limits = hms::hms(hours = c(17, 21)),
    labels = scales::label_time(format = "%H:%M")
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 24, base_family = body_font) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.text.x = element_text(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0.1, "lines"),
  )

# join
ggdraw(p_main) +
  cowplot::draw_plot(p_inset,
    x = 0.62, width = 0.35,
    y = 0.08, height = 0.42
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-04-09", paste0("20240409", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
