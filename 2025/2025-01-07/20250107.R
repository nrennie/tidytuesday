# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(cowplot)
library(grid)


# Load data ---------------------------------------------------------------

talks <- readr::read_csv("2025/2025-01-07/data/cpd.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Fraunces")
font_add_google("Commissioner")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- nrBrand::nr_light
text_col <- nrBrand::nr_dark
highlight_col <- nrBrand::nr_mid

body_font <- "Commissioner"
title_font <- "Fraunces"


# Data wrangling ----------------------------------------------------------

plot_data <- talks |>
  filter(
    Type == "Presentation / Workshop",
    Year == 2024
  ) |>
  mutate(
    Date = mdy(Date)
  ) |>
  group_by(Date) |>
  mutate(
    n = row_number()
  ) |>
  ungroup() |>
  arrange(Date, desc(n)) |>
  mutate(
    month = month(Date, label = TRUE),
    dom = mday(Date)
  ) |>
  mutate(
    Topic = case_when(
      !(Topic %in% c("R", "Statistics", "Data Visualisation")) ~ "other",
      TRUE ~ Topic
    )
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-01-07", "recording"),
  device = "png",
  width = 5,
  height = 7,
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

col_palette <- PrettyCols::prettycols("Disco")[c(1, 3:5)]
names(col_palette) <- unique(plot_data$Topic)

title <- "Talks and Workshops in 2024"
st <- glue("During 2024, I delivered {nrow(plot_data)} talks or workshops (excluding lectures) on topics such as <span style='color:{col_palette[2]}'>**{names(col_palette[2])}**</span>, <span style='color:{col_palette[3]}'>**{names(col_palette[3])}**</span>, and <span style='color:{col_palette[4]}'>**{names(col_palette[4])}**</span> as well as <span style='color:{col_palette[1]}'>**{names(col_palette[1])}**</span> topics.")
cap <- paste0(
  "**Graphic**:", social
)


# Plot --------------------------------------------------------------------

p <- ggplot() +
  geom_point(
    data = plot_data,
    mapping = aes(
      x = dom,
      y = 1,
      size = n
    ),
    colour = "white"
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(
      x = dom,
      y = 1,
      size = n,
      fill = Topic
    ),
    alpha = 0.7,
    pch = 21,
    colour = text_col,
    linewidth = 0.5
  ) +
  facet_wrap(~month, ncol = 1, drop = FALSE, strip.position = "left") +
  scale_x_continuous(
    breaks = 1:31,
    limits = c(1, 31),
    expand = c(0, 0)
  ) +
  scale_size(range = c(3, 6)) +
  scale_fill_manual(
    values = col_palette
  ) +
  labs(
    x = "",
    y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = body_font, base_size = 10) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.background = element_rect(
      fill = alpha(highlight_col, 0.1),
      colour = NA
    ),
    strip.text.y.left = element_text(
      family = title_font,
      colour = text_col,
      angle = 0
    ),
    strip.background = element_rect(
      fill = alpha(highlight_col, 0.2),
      colour = NA
    ),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      colour = text_col,
      size = rel(0.8)
    ),
    axis.ticks.x = element_line(
      colour = text_col,
      linewidth = 0.5
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = alpha(highlight_col, 0.3),
      linewidth = 0.2
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.4)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 0),
      lineheight = 0.6,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 0),
      lineheight = 0.5,
      family = body_font
    )
  )


ggdraw(p) +
  draw_text(
    x = 0.33, y = 0.26,
    size = 8,
    colour = text_col,
    family = body_font,
    text = "RSS Conference\n2024"
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.28, y1 = 0.24,
      x2 = 0.2, y2 = 0.29,
      curvature = -0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.05, "inches"))
    )
  ) +
  draw_text(
    x = 0.7, y = 0.5,
    size = 8,
    colour = text_col,
    family = body_font,
    text = "Data viz session &\nNHS-R {tidymodels}\nworkshop"
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.6, y1 = 0.53,
      x2 = 0.68, y2 = 0.58,
      curvature = -0.4,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.05, "inches"))
    )
  ) +
  draw_text(
    x = 0.75, y = 0.85,
    size = 8,
    colour = text_col,
    family = body_font,
    text = "Rust RSE lightning talk\n& F4SG GAMs\nworkshop"
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.8, y1 = 0.81,
      x2 = 0.72, y2 = 0.78,
      curvature = -0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.05, "inches"))
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-01-07", paste0("20250107", ".png")),
  height = 7,
  width = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-01-07", paste0("20250107", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
