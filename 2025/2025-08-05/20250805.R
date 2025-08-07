# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-08-05")
income_inequality_processed <- tuesdata$income_inequality_processed
income_inequality_raw <- tuesdata$income_inequality_raw


# Load fonts --------------------------------------------------------------

font_add_google("Noto Sans", "noto")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "black"
highlight_col <- "#CC3F0C"

body_font <- "noto"
title_font <- "noto"


# Data wrangling ----------------------------------------------------------

plot_data <- income_inequality_processed |>
  mutate(gini_diff = gini_mi_eq - gini_dhi_eq) |>
  pivot_longer(starts_with("gini_")) |>
  mutate(
    name = case_when(
      name == "gini_mi_eq" ~ "Pre-tax",
      name == "gini_dhi_eq" ~ "Post-tax",
      name == "gini_diff" ~ "Difference"
    )
  )

country_levels <- income_inequality_processed |>
  mutate(gini_diff = gini_mi_eq - gini_dhi_eq) |>
  group_by(Entity) |>
  arrange(desc(gini_diff), gini_dhi_eq) |>
  slice_head(n = 1) |>
  ungroup() |>
  arrange(desc(gini_diff), desc(gini_dhi_eq)) |>
  pull(Entity)
plot_data$Entity <- factor(
  plot_data$Entity,
  levels = country_levels
)

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-08-05", "recording"),
  device = "png",
  width = 9,
  height = 6,
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
title <- "Ireland, Belgium, Sweden, and Germany are leaders in using taxation to reduce income inequality"
st <- "The Gini coefficient measures inequality on a scale from 0 to 1. Higher values indicate higher inequality. Inequality is measured here in terms of income before and after taxes and benefits. Though no countries listed here directly increase inequality through taxation, the level of inequality measured before and after tax still differs substantially between countries.*"
cap <- paste0(
  "*Pre- and post- tax coefficients are not available for all countries during all time periods.<br>
  **Data**: Our World in Data | **Graphic**:", social
)


# Functions ---------------------------------------------------------------

curved_arrow <- function(x_start, x_end, y_start, y_end, color = text_col, ...) {
  annotate(
    geom = "curve",
    x = x_start,
    xend = x_end,
    y = y_start,
    yend = y_end,
    linewidth = 0.3,
    color = color,
    arrow = arrow(
      length = unit(1.5, "mm"), type = "closed"
    ),
    ...
  )
}


# Plot --------------------------------------------------------------------

# legend plot
p_legend <- ggplot() +
  geom_area(
    data = filter(plot_data, Code == "AUS", name != "Pre-tax"),
    mapping = aes(x = Year, y = value, fill = name)
  ) +
  # country value
  geom_text(
    data = filter(plot_data, Code == "AUS", name == "Difference") |>
      slice_max(Year, n = 1),
    mapping = aes(x = 2022, y = 0.95, label = Entity),
    colour = text_col,
    size = 4,
    fontface = "bold",
    family = body_font,
    hjust = 1
  ) +
  # end of line annotations
  geom_line(
    data = filter(plot_data, Code == "AUS", name != "Difference"),
    mapping = aes(x = Year, y = value, group = name),
    colour = "#9A3009",
    linewidth = 1
  ) +
  geom_text(
    data = filter(plot_data, Code == "AUS", name != "Difference") |> slice_max(Year, n = 1),
    mapping = aes(
      x = Year + 1, y = value,
      label = str_wrap(name, 4, whitespace_only = FALSE)
    ),
    colour = "#9A3009",
    size = 3,
    fontface = "bold",
    family = body_font,
    hjust = 0,
    lineheight = 0.6
  ) +
  geom_point(
    data = filter(plot_data, Code == "AUS", name != "Difference") |> slice_max(Year, n = 1),
    mapping = aes(x = Year, y = value),
    colour = "#9A3009",
    size = 2
  ) +
  # annotations
  annotate("text",
    x = 1963.5, y = 0.95,
    label = str_wrap("\u2191 Higher inequality", 20),
    colour = text_col, family = body_font,
    size = 3,
    hjust = 0,
    lineheight = 0.9
  ) +
  annotate("text",
    x = 1963.5, y = 0.05,
    label = str_wrap("\u2193 Lower inequality", 20),
    colour = text_col, family = body_font,
    size = 3,
    hjust = 0,
    lineheight = 0.9
  ) +
  annotate("text",
    x = 1985, y = 0.65,
    label = str_wrap("Reduction in inequality due to taxation", 12),
    colour = highlight_col, family = body_font,
    fontface = "bold",
    size = 3.5,
    lineheight = 0.9
  ) +
  curved_arrow(1985, 1986, 0.5, 0.4,
    curvature = 0.4, color = highlight_col
  ) +
  # styling
  scale_fill_manual(
    values = c("#CC3F0C", "#F79A78"), guide = "none"
  ) +
  scale_x_continuous(limits = range(income_inequality_processed$Year),
                     expand = expansion(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE,
    clip = "off") +
  labs(x = NULL, y = NULL, subtitle = "Gini coefficient") +
  theme_bw(base_family = body_font) +
  theme(
    plot.margin = margin(40, 25, 10, 10),
    axis.ticks = element_blank(),
    plot.subtitle = element_text(face = "bold"),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = "grey93", colour = NA),
    panel.border = element_blank(),
    axis.line.y.left = element_line(colour = text_col, linewidth = 0.3),
    axis.line.x.bottom = element_line(colour = text_col, linewidth = 0.3)
  )

# faceted plot
p_main <- ggplot() +
  geom_area(
    data = filter(plot_data, name != "Pre-tax"),
    mapping = aes(x = Year, y = value, fill = name)
  ) +
  geom_text(
    data = filter(plot_data, name == "Difference") |>
      group_by(Entity) |>
      slice_max(Year, n = 1) |>
      ungroup(),
    mapping = aes(x = 2022, y = 0.95, label = str_wrap(Entity, 12)),
    colour = text_col,
    size = 2.5,
    fontface = "bold",
    family = body_font,
    hjust = 1,
    vjust = 1,
    lineheight = 0.8
  ) +
  labs(
    title = title, subtitle = st,
    caption = cap
  ) +
  facet_wrap(~Entity, nrow = 4) +
  scale_fill_manual(
    values = c("#CC3F0C", "#F79A78"), guide = "none"
  ) +
  scale_x_continuous(limits = range(income_inequality_processed$Year)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    strip.text = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = "grey93", colour = NA),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.8,
      family = title_font,
      face = "bold",
      size = rel(1.4),
      maxwidth = 0.5
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
      lineheight = 1,
      family = body_font,
      size = rel(1.1),
      maxwidth = 0.4
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      lineheight = 1,
      family = body_font
    ),
    panel.spacing = unit(0.2, "lines")
  )


# join
p_main + inset_element(p_legend, 0.5, 0.58, 1.00, 1.08,
  align_to = "full", clip = FALSE
)


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-08-05", paste0("20250805", ".png")),
  width = 9,
  height = 6,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-08-05", paste0("20250805", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
