# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggh4x)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-06-23")
encyclicals <- tuesdata$encyclicals
papal_encyclicals <- tuesdata$papal_encyclicals
scripture_references <- tuesdata$scripture_references


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
highlight_col <- "#991E43"


# Data wrangling ----------------------------------------------------------

pope_data <- papal_encyclicals |>
  select(pope, papal_number, birth_name, birth_country, pontificate_start, pontificate_end) |>
  distinct() |>
  arrange(papal_number) |>
  mutate(
    start_year = year(pontificate_start),
    end_year = year(pontificate_end),
    pope_label = paste0("**", pope, "**<br>Born ", birth_name, "<br><i>", birth_country, "</i>")
  ) |>
  select(-c(pontificate_start, pontificate_end, birth_name, birth_country)) |>
  mutate(
    pope = factor(pope, pope),
    pope_label = factor(pope_label, pope_label)
  ) |>
  mutate(
    end_year_text = if_else(
      is.na(end_year), "Present Day", as.character(end_year)
    ),
    end_year = if_else(
      is.na(end_year), year(today()), end_year
    )
  )

plot_data <- papal_encyclicals |>
  select(pope, pontificate_year) |>
  group_by(pope, pontificate_year) |>
  mutate(n = n()) |>
  ungroup() |>
  distinct() |>
  left_join(pope_data, by = "pope") |>
  mutate(pope_label = factor(pope_label, pope_data$pope_label))

avg_data <- plot_data |>
  group_by(pope_label) |>
  mutate(
    n = sum(n),
    year_diff = end_year - start_year
  ) |>
  ungroup() |>
  select(pope_label, n, year_diff) |>
  mutate(avg_n = n / year_diff) |>
  distinct()


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "The number of papal encyclicals is decreasing"
st <- "A *papal encyclical* is a formal letter from the Pope, typically addressed to bishops or the wider Catholic Church, that sets out official teaching on matters of doctrine, morals, or social issues."
cap <- paste0("**Note**: Each dot represents a year in which a papal encyclicals was published, with the size of the dot representing the number published that year.<br>", source_caption(source = "Vatican.va", graphic = social))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_segment(
    data = pope_data,
    mapping = aes(
      x = 1, xend = end_year - start_year + 1,
      y = pope_label
    ),
    colour = highlight_col,
    alpha = 0.25,
    linewidth = 6,
    lineend = "round"
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(x = 1, y = pope_label, label = start_year),
    family = body_font,
    size = 3,
    vjust = -1.2,
    hjust = 1
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(
      x = end_year - start_year + 1,
      y = pope_label, label = end_year_text
    ),
    family = body_font,
    size = 3,
    vjust = -1.2,
    hjust = 0
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(x = pontificate_year, y = pope_label, size = n),
    colour = highlight_col
  ) +
  geom_point(
    data = avg_data,
    mapping = aes(x = 36, y = pope_label, size = avg_n),
    colour = text_col
  ) +
  geom_text(
    data = avg_data,
    mapping = aes(x = 36.6, y = pope_label, label = round(avg_n, 1)),
    family = body_font,
    hjust = 0,
    size = 3
  ) +
  at_panel(annotate("text",
    x = 36, y = 4, label = "Avg / year",
    size = 3.2, fontface = "bold", colour = text_col
  ), PANEL %in% c(1)) +
  facet_wrap(~pope_label,
    ncol = 1,
    scales = "free_y", strip.position = "left"
  ) +
  scale_x_continuous(
    breaks = seq(5, 30, 5),
    limits = c(-1, 37)
  ) +
  scale_y_discrete(limits = rev) +
  scale_size_area(max_size = 4.5) +
  labs(
    x = "Year of pontification", y = NULL,
    title = title, subtitle = st,
    caption = cap
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
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
      margin = margin(b = 5, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text.y.left = element_textbox(
      margin = margin(t = 10),
      size = rel(1),
      hjust = 0,
      vjust = 1.5,
      valign = 1.5,
      halign = 0
    ),
    strip.clip = "off",
    axis.title.x = element_text(hjust = 0.72, colour = alpha(text_col, 0.7)),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  ) +
  canvas(
    width = 7, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-06-23", paste0("20260623", ".png"))
)
