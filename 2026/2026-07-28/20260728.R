# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(rnaturalearth)
library(sf)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-07-28")
occurrences <- tuesdata$occurrences
tourism <- tuesdata$tourism
weather <- tuesdata$weather


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
font_add("FA",
  regular = "fonts/Font Awesome 6 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#151C28"
text_col <- "#F2F4F8"
highlight_col <- "#F514B9"


# Data wrangling ----------------------------------------------------------

icon_up <- "<span style='font-size:8pt;font-family:\"FA\";'>&#xf062;</span> "
icon_down <- "<span style='font-size:8pt;font-family:\"FA\";'>&#xf063;</span> "

plot_data <- occurrences |>
  filter(organism_name == "Orchid") |>
  mutate(genus = stringr::str_extract(sci_name, "^[^ ]+")) |>
  group_by(year, genus) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(genus) |>
  mutate(tot_count = sum(n)) |>
  filter(tot_count >= 30) |>
  ungroup() |>
  select(-tot_count) |>
  complete(year, genus, fill = list(n = 0))

change_data <- plot_data |>
  group_by(genus) |>
  mutate(max_n = max(n)) |>
  ungroup() |>
  filter(year %in% c(2023, 2024)) |>
  pivot_wider(names_from = year, values_from = n) |>
  mutate(
    change = `2024` - `2023`,
    perc = round(100 * change / `2023`)
  ) |>
  mutate(
    perc_label = case_when(
      perc > 0 ~ glue("{icon_up} {perc}%"),
      perc < 0 ~ glue("{icon_down} {abs(perc)}%"),
      perc == 0 ~ glue("No change"),
    )
  ) |>
  arrange(desc(perc)) |>
  mutate(genus = factor(genus, levels = genus))

plot_data$genus <- factor(plot_data$genus, levels = levels(change_data$genus))

obs_states <- occurrences |>
  filter(organism_name == "Orchid") |>
  pull(obs_state) |>
  unique()

states <- ne_states(country = "Australia", returnclass = "sf")
wa <- subset(states, name == "Western Australia")
wa <- st_transform(wa, st_crs(ecotourism::oz_lga))
wa_lgas <- ecotourism::oz_lga[st_within(ecotourism::oz_lga, wa, sparse = FALSE), ]
wa_regions <- wa_lgas$NAME |>
  str_to_lower() |>
  str_remove(" \\([A-Za-z]+\\)$")
trip_data <- tourism |>
  filter(str_to_lower(region) %in% wa_regions) |>
  group_by(year, purpose) |>
  summarise(trips = sum(trips), .groups = "drop")


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Orchid observations are increasing in Western Australia"
st <- "<span style='font-size:10pt;'>Number of observations of orchid genera per year between 2014 and 2024.<br>Percentage change between 2023 and 2024.</span>"
cap <- paste0(
  "**Note**: Only genera with at least 30 observations are included. An increasing number of observations does not necessarily mean an increasing number of orchids, as observation numbers are reliant on the presence of humans.<br>",
  source_caption(
    source = "Atlas of Living Australia via {ecotourism}",
    graphic = social
  )
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_area(
    data = plot_data,
    mapping = aes(x = year, y = n),
    fill = highlight_col,
    alpha = 0.15
  ) +
  geom_line(
    data = plot_data,
    mapping = aes(x = year, y = n),
    colour = highlight_col
  ) +
  geom_point(
    data = filter(plot_data, year == 2024),
    mapping = aes(x = year, y = n),
    colour = highlight_col
  ) +
  geom_richtext(
    data = change_data,
    mapping = aes(x = 2014, y = max_n, label = perc_label),
    hjust = 0,
    vjust = 1,
    colour = text_col,
    label.margin = unit(c(0.2, 0, 0, 0), "lines"),
    label.r = unit(0, "lines"),
    label.colour = "transparent",
    fill = alpha(bg_col, 0.5),
    fontface = "bold"
  ) +
  facet_wrap(~genus, scales = "free_y", ncol = 4, axes = "all_x") +
  scale_x_continuous(
    limits = c(2014, 2024),
    breaks = c(2014, 2024),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = scales::label_comma()
  ) +
  labs(
    x = NULL, y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    text = element_text(colour = text_col),
    plot.margin = margin(5, 15, 5, 5),
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
      margin = margin(b = 5, t = 5),
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
      colour = text_col,
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    axis.text.x = element_text(colour = alpha(text_col, 0.5)),
    axis.text.y = element_text(colour = alpha(text_col, 0.4)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha(text_col, 0.3), linewidth = 0.1
    ),
    panel.grid.minor = element_blank()
  ) +
  canvas(
    width = 6, height = 10,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-07-28", paste0("20260728", ".png"))
)
