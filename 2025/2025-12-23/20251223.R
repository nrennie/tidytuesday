# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggalluvial)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-12-23")
endangered_status <- tuesdata$endangered_status
families <- tuesdata$families
languages <- tuesdata$languages


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#d7191c"


# Data wrangling ----------------------------------------------------------

plot_data <- languages |>
  filter(str_detect(countries, "GB")) |>
  left_join(endangered_status, by = "id") |>
  left_join(families, by = c("family_id" = "id")) |>
  select(family, status_label, status_code, name) |>
  drop_na() |>
  arrange(status_code) |>
  mutate(
    status_label = str_to_sentence(status_label),
    status_label = str_wrap(status_label, 12)
  ) |>
  mutate(n = 1) |>
  mutate(
    name = factor(name, levels = name)
  )

plot_data$status_label <- factor(plot_data$status_label,
  levels = unique(plot_data$status_label)
)

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-12-23", "recording"),
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
  font_family = body_font,
  mastodon = NA
)
title <- "Celtic languages remain under threat of extinction"
st <- "Glottolog is the most comprehensive language database in linguistics and it contains information on over 8,000 languages of the world. Excluding artificial languages, 25 languages are listed as being spoken in Great Britain. The endangered status of a language is based on global speakers rather than only those within Great Britain."
cap <- paste0(
  "**Data**: Glottolog 5.2.1<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(
    axis1 = family,
    axis2 = name,
    axis3 = status_label,
    y = n
  )
) +
  geom_alluvium(aes(fill = status_label)) +
  geom_stratum(
    mapping = aes(width = case_when(
      after_stat(x) == 1 ~ 0.4,
      after_stat(x) == 2 ~ 0,
      after_stat(x) == 3 ~ 0.35
    ),
    colour = as.character(after_stat(x))),
    fill = bg_col
  ) +
  geom_text(
    stat = "stratum",
    mapping = aes(
      label = after_stat(stratum),
      x = stage(
        after_stat(x),
        after_scale = case_when(
          x == 1 ~ x - 0.2 + 0.02,
          x == 2 ~ x - 0.05,
          x == 3 ~ x - 0.175 + 0.02
        )
      )
    ),
    family = body_font,
    colour = text_col,
    hjust = 0,
    size = 3.5
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_colour_manual(
    values = c("grey70", "transparent", "grey70")
  ) +
  scale_fill_manual(
    values = c(rev(c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6")),
               bg_col, "transparent", bg_col)
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-12-23", paste0("20251223", ".png")),
  width = 7,
  height = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-12-23", paste0("20251223", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
