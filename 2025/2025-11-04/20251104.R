# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggdist)
library(cowplot)
library(grid)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-11-04")
flint_mdeq <- tuesdata$flint_mdeq
flint_vt <- tuesdata$flint_vt


# Load fonts --------------------------------------------------------------

font_add_google("Libre Franklin", "libre")
font_add_google("Domine", "domine")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "grey99"
text_col <- "black"
highlight_col <- "#A4243B"

body_font <- "libre"
title_font <- "domine"


# Data wrangling ----------------------------------------------------------


mdeq_data <- flint_mdeq |>
  select(lead, lead2) |>
  pivot_longer(cols = everything()) |>
  mutate(
    name = case_when(
      name == "lead" ~ "Samples collected by Michigan Department of Environment\n(before sample exclusion)",
      name == "lead2" ~ "Samples collected by Michigan Department of Environment\n(after sample exclusion)"
    )
  )

vt_data <- flint_vt |>
  select(lead) |>
  rename(value = lead) |>
  mutate(name = "Samples collected by Virginia Tech")

plot_data <- rbind(mdeq_data, vt_data)
plot_data$name <- factor(plot_data$name,
                         levels = c(
                           "Samples collected by Michigan Department of Environment\n(before sample exclusion)",
                           "Samples collected by Michigan Department of Environment\n(after sample exclusion)",
                           "Samples collected by Virginia Tech"))

summary_data <- plot_data |>
  group_by(name) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    perc_90 = quantile(value, probs = 0.9, na.rm = TRUE)
  )

flint_mdeq |> 
  filter(!is.na(notes))
plot_data |> 
  filter(name == "Samples collected by Michigan Department of Environment\n(before sample exclusion)", value > 37)

annotation1 <- "Two samples with identical value of 42 remain in data"
annotation2 <- "Sample with value of 20 removed as *'business not residence'*"
annotation3 <- "Sample with value of 104 removed as *'house had a filter'*"

note_data <- data.frame(
  x = c(70, 65, 140),
  y = rep(0.5, 3),
  name = c("Samples collected by Michigan Department of Environment\n(after sample exclusion)",
           "Samples collected by Michigan Department of Environment\n(before sample exclusion)", 
           "Samples collected by Michigan Department of Environment\n(before sample exclusion)"),
  label = c(annotation1, annotation2, annotation3)
)
note_data$name <- factor(note_data$name,
                         levels = c(
                           "Samples collected by Michigan Department of Environment\n(before sample exclusion)",
                           "Samples collected by Michigan Department of Environment\n(after sample exclusion)",
                           "Samples collected by Virginia Tech"))

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-11-04", "recording"),
  device = "png",
  width = 7,
  height = 8,
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
title <- "Citizens and scientists challenge findings"
st <- "Samples were collected in Flint, Michigan in 2015 by the Michigan Department of Environment (MDEQ) to explore lead levels in water. Community-sourced samples were also later collected through a citizen science project coordinated by Prof Marc Edwards and colleagues at Virginia Tech, after concerns were raised about the MDEQ excluding samples from their data.<br><br>If the 90th percentile value of the sample is above 15 ppb, action is required. The removal of two values from the MDEQ data resulted in the 90th percentile dropping below the 15 ppb threshold."
cap <- paste0(
  "**Data**: Loux and Gibson (2018)<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

p <- ggplot(
  data = plot_data,
) +
  annotate(
    "rect",
    xmin = 0, xmax = 160,
    ymin = 0, ymax = 1,
    fill = "white",
    alpha = 0.1,
    colour = text_col
  ) +
  geom_rect(
    data = plot_data,
    mapping = aes(
      ymin = 0, ymax = 1,
      xmin = value - 0.5, xmax = value + 0.5,
      fill = name
    ),
    colour = "transparent",
    alpha = 0.3
  ) +
  geom_segment(
    data = summary_data,
    mapping = aes(
      x = mean, y = 0, yend = 1
    ),
    colour = text_col,
    linewidth = 1.5
  ) +
  geom_text(
    data = summary_data,
    mapping = aes(
      x = mean, y = 1.2,
      label = paste0("Mean\n", round(mean, 1), " ppb")
    ),
    hjust = 0,
    size = 3.5,
    lineheight = 0.9,
    colour = text_col,
    family = body_font
  ) +
  geom_segment(
    data = summary_data,
    mapping = aes(
      x = median, y = 0, yend = 1
    ),
    colour = text_col,
    linewidth = 1.5
  ) +
  geom_text(
    data = summary_data,
    mapping = aes(
      x = median, y = 1.2,
      label = paste0("Median\n", round(median, 1), " ppb")
    ),
    hjust = 1,
    size = 3.5,
    lineheight = 0.9,
    colour = text_col,
    family = body_font
  ) +
  geom_segment(
    data = summary_data,
    mapping = aes(
      x = perc_90, y = 0, yend = 1
    ),
    colour = text_col,
    linewidth = 1.5
  ) +
  geom_text(
    data = summary_data,
    mapping = aes(
      x = perc_90 + 7, y = 1.2,
      label = paste0("90th percentile\n", round(perc_90, 1), " ppb")
    ),
    hjust = 0,
    size = 3.5,
    lineheight = 0.9,
    colour = text_col,
    family = body_font,
    fontface = "bold"
  ) +
  geom_textbox(
    data = note_data,
    mapping = aes(x = x, y = y, label = label),
    fill = "transparent",
    family = body_font,
    colour = text_col,
    size = 3.5,
    box.colour = "transparent",
    hjust = 0.5, halign = 0,
    vjust = 0.5, valign = 0.5,
    maxwidth = unit(1.5, "in")
  ) +
  labs(
    x = "Lead level (ppb, parts per billion)",
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_fill_manual(values = c("#A4243B", "#A4243B", "#268EBA")) +
  scale_x_continuous(limits = c(-7, 160)) +
  scale_y_continuous(limits = c(0, 1.4)) +
  facet_wrap(~name, ncol = 1) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_grey(base_family = body_font) +
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
      size = rel(1.8)
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
    axis.title.y = element_blank(),
    axis.title.x = element_text(hjust = 1),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(
      hjust = 1,
      vjust = 0,
      face = "bold",
      size = rel(0.9),
      family = title_font,
      lineheight = 1.1,
      margin = margin(b = -20, t = 15)
    )
  )

ggdraw(p) +
  draw_grob(
    curveGrob(
      x1 = 0.35, y1 = 0.4,
      x2 = 0.32, y2 = 0.4,
      curvature = -0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.75, y1 = 0.62,
      x2 = 0.68, y2 = 0.62,
      curvature = -0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.35, y1 = 0.56,
      x2 = 0.19, y2 = 0.53,
      curvature = -0.5,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.21, y1 = 0.68,
      x2 = 0.18, y2 = 0.65,
      curvature = -0.5,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.17, y1 = 0.45,
      x2 = 0.14, y2 = 0.42,
      curvature = -0.5,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.27, y1 = 0.22,
      x2 = 0.24, y2 = 0.19,
      curvature = -0.5,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-11-04", paste0("20251104", ".png")),
  width = 7,
  height = 8,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-11-04", paste0("20251104", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
