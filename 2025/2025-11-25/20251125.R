# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Parameters --------------------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-11-25")
spi_indicators <- tuesdata$spi_indicators


# Data wrangling ----------------------------------------------------------

plot_data <- spi_indicators |>
  select(
    country, income, year, overall_score
  ) |>
  filter(
    year >= 2016,
    income != "Not classified"
  ) |>
  mutate(income = factor(income, levels = c(
    "High income", "Upper middle income",
    "Lower middle income", "Low income"
  )))

summary_data <- plot_data |>
  group_by(year, income) |>
  summarise(value = median(overall_score, na.rm = TRUE)) |>
  ungroup()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-11-25", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
source_caption <- function(source, sep = "<br>", graphic = social) {
  glue::glue(
    "**Data**: {source} {sep} **Graphic**: {graphic}"
  )
}

title <- "Statistical performance is improving, but remains related to income"
st <- glue("The World Bank's Statistical Performance Indicators were developed to help countries assess and improve the performance of their statistical systems. Multiple indicators across data use, data services, data products, data sources, and data infrastructure are combined to give an overall score, with 100 being the highest score.<br><br>The <span style='color:{highlight_col};'>**median score**</span> across all countries within an income bracket has been consistently increasing. However, lower income countries still show poorer statistical performance on average.<br>")
cap <- source_caption("Statistical Performance Indicators from The World Bank")


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = year, y = overall_score)
) +
  geom_line(
    mapping = aes(group = country),
    colour = alpha("grey65", 0.6),
    linewidth = 0.4
  ) +
  geom_line(
    data = summary_data,
    mapping = aes(x = year, y = value),
    colour = highlight_col,
    linewidth = 1.1
  ) +
  geom_point(
    data = filter(summary_data, year == 2023),
    mapping = aes(x = year, y = value),
    colour = highlight_col,
    size = 2
  ) +
  geom_text(
    data = filter(summary_data, year == 2023),
    mapping = aes(x = year, y = value, label = round(value)),
    colour = highlight_col,
    fontface = "bold",
    size = 4,
    hjust = -0.4
  ) +
  geom_hline(yintercept = 0, colour = "grey65") +
  facet_wrap(~income, nrow = 1) +
  scale_x_continuous(breaks = c(2017, 2020, 2023)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = NULL,
    y = "Score"
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    axis.title.y = element_text(
      angle = 0,
      vjust = 1.1,
      hjust = 1,
      margin = margin(r = -16)
    ),
    panel.spacing = unit(1.3, "lines"),
    plot.margin = margin(5, 20, 5, 5),
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
    panel.grid.minor = element_blank()
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-11-25", paste0("20251125", ".png")),
  width = 7,
  height = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-11-25", paste0("20251125", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
