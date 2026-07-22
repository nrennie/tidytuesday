# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-07-21")
nde_experiences <- tuesdata$nde_experiences


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

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"
comp_col <- "#197176"


# Data wrangling ----------------------------------------------------------

offset <- 5
plot_data <- nde_experiences |>
  filter(classification == "NDE") |>
  group_by(gender, greyson_score) |>
  drop_na(gender, greyson_score) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(
    xmin = if_else(gender == "M", -offset, offset),
    xmax = if_else(gender == "M", (-1 * n) - offset, n + offset)
  ) |>
  mutate(greyson_score = factor(greyson_score, levels = 0:32)) |>
  complete(gender, greyson_score) |>
  mutate(
    n = if_else(
      is.na(n), 0, n
    ),
    xmin = case_when(
      is.na(xmin) & gender == "F" ~ offset,
      is.na(xmin) & gender == "M" ~ -offset,
      TRUE ~ xmin
    ),
    xmax = case_when(
      is.na(xmax) & gender == "F" ~ offset,
      is.na(xmax) & gender == "M" ~ -offset,
      TRUE ~ xmax
    )
  ) |>
  mutate(
    greyson_score = as.character(greyson_score),
    greyson_score = as.numeric(greyson_score)
  )

num_nde <- plot_data |>
  filter(greyson_score >= 7) |>
  summarise(n = sum(abs(n))) |>
  pull(n)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Most near death experiences aren't actually near death experiences"
st <- glue("Near-death experiences (NDEs) typically involve out-of-body perception, a feeling of peace, seeing a bright light, or encountering deceased relatives. The Greyson NDE Scale is the standard validated instrument for measuring NDE depth. It ranges from 0 to 32, and a score of 7 or higher indicates a genuine NDE. Of the {sum(abs(plot_data$n))} experiences classified as an NDE by NDERF Search, only {num_nde} had a Greyson score of 7 or above.")
cap <- source_caption(source = "Near Death Experience Research Foundation", graphic = social)

icon_up <- "<span style='font-size:8pt;font-family:\"FA\";'>&#xf062;</span> "
icon_down <- "<span style='font-size:8pt;font-family:\"FA\";'>&#xf063;</span> "


# Plot --------------------------------------------------------------------

ggplot() +
  annotate("rect",
    xmin = -100, xmax = 100,
    ymin = 6.5, ymax = 32.55,
    fill = text_col, alpha = 0.15
  ) +
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = xmin, xmax = xmax, ymin = greyson_score - 0.45,
      ymax = greyson_score + 0.45
    ),
    fill = bg_col
  ) +
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = xmin, xmax = xmax, ymin = greyson_score - 0.45,
      ymax = greyson_score + 0.45, fill = gender
    ),
    alpha = 0.7
  ) +
  geom_step(
    data = rbind(
      plot_data,
      plot_data |>
        group_by(gender) |>
        slice_tail(n = 1) |>
        mutate(greyson_score = greyson_score + 1)
    ),
    mapping = aes(x = xmax, y = greyson_score - 0.45, colour = gender),
    orientation = "y",
    linewidth = 1,
    lineend = "round"
  ) +
  geom_text(
    data = data.frame(
      x = 0, y = -1:32, label = c("Greyson score", 0:32)
    ),
    mapping = aes(x = x, y = y, label = label)
  ) +
  # geom_hline(yintercept = 6.5) +
  # labels
  annotate("text",
    x = 95, y = 31, label = "Women",
    colour = highlight_col, size = 5, fontface = "bold",
    family = body_font, hjust = 1
  ) +
  annotate("text",
    x = -95, y = 31, label = "Men",
    colour = comp_col, size = 5, fontface = "bold",
    family = body_font, hjust = 0
  ) +
  geom_richtext(
    data = data.frame(
      x = c(-99, -99),
      y = c(6.2, 6.8),
      label = c(paste0(icon_up, "Not a Near Death Experience"),
                paste0(icon_down, "Near Death Experience")),
      vjust = c(0, 1)
    ),
    mapping = aes(
      x = x, y = y, label = label,
      vjust = vjust,
      valign = vjust
    ),
    hjust = 0,
    size = 3.5,
    colour = text_col,
    label.colour = "transparent",
    fill = alpha(bg_col, 0),
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = c(-100, -50, 0, 50, 100),
    labels = c("100 men", 50, 0, 50, "100 women")
  ) +
  scale_y_reverse() +
  scale_fill_manual(
    values = c("F" = highlight_col, "M" = comp_col)
  ) +
  scale_colour_manual(
    values = c("F" = highlight_col, "M" = comp_col)
  ) +
  labs(
    x = "Number of experiences reported", y = NULL,
    title = title, subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 10),
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
      size = rel(1.2)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(hjust = 1),
    axis.text.y = element_blank(),
    axis.text.x = element_text(hjust = c(0, 0.5, 0.5, 0.5, 1)),
    axis.ticks.x = element_line(),
    panel.grid.major.y = element_blank()
  ) +
  canvas(
    width = 5, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-07-21", paste0("20260721", ".png"))
)
