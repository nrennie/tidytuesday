# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggpattern)
library(rcartocolor)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-01-21")
exped_tidy <- tuesdata$exped_tidy
peaks_tidy <- tuesdata$peaks_tidy


# Load fonts --------------------------------------------------------------

font_add_google("Sriracha", "sriracha")
font_add_google("Open Sans", "open")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#2d4c7b"
text_col <- "white"
col_high <- rcartocolor::carto_pal(n = 2, "Earth")[2]
col_low <- rcartocolor::carto_pal(n = 2, "Earth")[1]

body_font <- "open"
title_font <- "sriracha"


# Data wrangling ----------------------------------------------------------

peaks_data <- peaks_tidy |>
  select(PEAKID, HIMAL_FACTOR, HEIGHTM)

fatality_data <- exped_tidy |>
  select(PEAKID, TOTMEMBERS, MDEATHS) |>
  left_join(peaks_data, by = "PEAKID") |>
  group_by(HIMAL_FACTOR) |>
  summarise(
    n_people = sum(TOTMEMBERS),
    n_deaths = sum(MDEATHS),
    fatality_rate = n_deaths / n_people
  ) |>
  select(HIMAL_FACTOR, fatality_rate)

height_sort <- peaks_data |>
  group_by(HIMAL_FACTOR) |>
  summarise(avg_height = mean(HEIGHTM)) |>
  arrange(desc(avg_height)) |>
  pull(HIMAL_FACTOR)

plot_data <- peaks_data |>
  left_join(fatality_data, by = "HIMAL_FACTOR") |>
  mutate(
    HIMAL_FACTOR = factor(HIMAL_FACTOR, levels = height_sort)
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-01-21", "recording"),
  device = "png",
  width = 5,
  height = 9,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = col_high,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Taller mountain ranges are more dangerous"
st <- glue("The Himalayan Database is a comprehensive archive documenting mountaineering expeditions in the Nepal Himalaya. These data are rich in historical value, detailing the peaks, expeditions, climbing statuses, and geographic information of numerous Himalayan summits. Expeditions to mountains in ranges with a higher average height, results in <span style='color:{col_high}'>**more fatalities**</span>* compared to lower mountain ranges which results in <span style='color:{col_low}'>**fewer fatalities**</span> on average.<br><br>* Fatality rates are calculated from expeditions occurring between 2020 and 2024. <span style='color:#808080'>**grey areas**</span> indicate a range where no expeditions have taken placer during this time period. ")
cap <- paste0(
  "**Data**: Himalayan Database<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = HEIGHTM)
) +
  geom_density(
    mapping = aes(fill = fatality_rate),
    colour = "transparent"
  ) +
  geom_density_pattern(
    data = filter(plot_data, !is.na(fatality_rate)),
    pattern = "gradient",
    pattern_fill = NA,
    pattern_fill2 = "white",
    pattern_orientation = "vertical",
    fill = NA,
    colour = NA
  ) +
  scale_fill_carto_c(palette = "Earth", na.value = "#808080") +
  facet_wrap(~HIMAL_FACTOR,
    ncol = 1, strip.position = "left"
  ) +
  labs(
    x = "Peak height (m)",
    y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(
    base_size = 10, base_family = body_font
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = text_col),
    axis.title.x = element_text(colour = text_col),
    strip.text.y.left = element_text(
      family = title_font,
      colour = text_col,
      angle = 0,
      hjust = 1,
      vjust = 0,
    ),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.background = element_blank(),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.6)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = , t = 5),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-01-21", paste0("20250121", ".png")),
  height = 9,
  width = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-01-21", paste0("20250121", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
