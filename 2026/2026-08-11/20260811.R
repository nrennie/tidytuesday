# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggfx)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-08-11")
palomar_emission_lines <- tuesdata$palomar_emission_lines
palomar_survey <- tuesdata$palomar_survey


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("GFS Neohellenic", "GFS")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "GFS"


# Define colours and fonts-------------------------------------------------

bg_col <- "#151C28"
text_col <- "#F2F4F8"


# Data wrangling ----------------------------------------------------------

plot_data <- palomar_survey |>
  select(
    galaxy_name, activity_type, classification_confidence,
    velocity_dispersion_km_s, helio_velocity_km_s,
    log_nii_ha, log_oiii_hb
  ) |>
  drop_na() |>
  mutate(classification_confidence_alpha = case_when(
    classification_confidence == "very uncertain" ~ 0.3,
    classification_confidence == "uncertain" ~ 0.6,
    classification_confidence == "confident" ~ 0.9
  )) |>
  arrange(desc(helio_velocity_km_s))


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Nearby galaxies with active nuclei are more common than previously recognised"
st <- "In the 1990s, astronomers used the 200-inch Hale Telescope at Palomar Observatory — once the world's largest — to split the light from the centers of nearly 500 nearby galaxies into a rainbow of wavelengths. In this Palomar Spectroscopic Survey, by measuring the strength of specific emission lines in those spectra, they classified each galaxy's nucleus as powered by young stars, by an active galactic nucleus (AGN), or by some combination of both.<br><span style='font-size:9pt;'><br>[O III] λ5007 / Hβ flux ratio</span>"
cap <- source_caption(source = "Palomar Spectroscopic Survey", graphic = social, sep = " | ")


# Plot --------------------------------------------------------------------

chosen <- c("Seyfert", "Transition", "LINER")
annotate1 <- "Galaxies with nuclei powered by an active galactic nucleus that are <span style='color:#FFD449'>**moving towards us**</span>"
annotate2 <- "Galaxies with nuclei powered by an active galactic nucleus that are <span style='color:#8958C6'>**moving away from us**</span>"
annotate3 <- "Galaxies with nuclei powered by young stars"
annotate4 <- "Smaller circles indicate lower central stellar velocity dispersion, which correlates with central mass."
annotate5 <- "More transparent circles indicate more uncertainty in the classification."

ggplot(
  data = plot_data,
  mapping = aes(
    x = log_nii_ha,
    y = log_oiii_hb,
    size = velocity_dispersion_km_s,
    alpha = classification_confidence_alpha
  )
) +
  with_outer_glow(
    geom_point(
      fill = "#2f3540",
      colour = bg_col,
      pch = 21
    ),
    colour = "#7E8AA0",
    expand = 2
  ) +
  with_outer_glow(
    geom_point(
      data = filter(plot_data, activity_type %in% chosen),
      mapping = aes(fill = helio_velocity_km_s > 0),
      colour = bg_col,
      pch = 21
    ),
    colour = "#959FB1",
    expand = 2
  ) +
  geom_textbox(
    data = data.frame(
      x = c(0.4, 9, 0.3, 0.01, 0.8),
      y = c(15, 0.1, 0.02, 0.5, 50),
      hjust = c(1, 1, 0.5, 0, 0),
      label = c(annotate1, annotate2, annotate3, annotate4, annotate5)
    ),
    mapping = aes(
      x = x, y = y, label = label,
      hjust = hjust, halign = hjust
    ),
    fill = alpha(bg_col, 0.5),
    colour = text_col,
    box.colour = "transparent",
    inherit.aes = FALSE,
    family = body_font
  ) +
  # Arrows
  annotate("curve",
    x = 0.18, xend = 0.4,
    y = 7, yend = 4,
    arrow = arrow(
      length = unit(0.05, "inches"),
      type = "closed"
    ),
    curvature = -0.1,
    colour = text_col
  ) +
  annotate("curve",
    x = 5, xend = 3,
    y = 0.25, yend = 0.5,
    arrow = arrow(
      length = unit(0.05, "inches"),
      type = "closed"
    ),
    curvature = 0.1,
    colour = text_col
  ) +
  annotate("curve",
    x = 0.17, xend = 0.25,
    y = 0.035, yend = 0.055,
    arrow = arrow(
      length = unit(0.05, "inches"),
      type = "closed"
    ),
    curvature = -0.1,
    colour = text_col
  ) +
  # Styling
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10),
    limits = c(0.01, 10),
    expand = expansion(0, 0)
  ) +
  scale_y_log10(
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c(0.01, 0.1, 1, 10, 100),
    limits = c(0.01, 100),
    expand = expansion(0, 0)
  ) +
  scale_alpha_identity() +
  scale_fill_manual(
    values = c("#FFD449", "#6B39A7")
  ) +
  scale_size(range = c(0.5, 4)) +
  labs(
    x = "<span style='font-size:9pt;'>[N II] λ6583 / Hα flux ratio</span>",
    y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
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
      size = rel(1.15)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 13, t = 5),
      family = body_font,
      width = 1.04
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
      margin = margin(t = 10, b = 5),
      size = rel(0.9),
      colour = text_col
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.3,
      colour = alpha(text_col, 0.2)
    ),
    panel.grid.major.y = element_line(
      linewidth = 0.3,
      colour = alpha(text_col, 0.2)
    ),
    panel.spacing.x = unit(1.5, "lines"),
    axis.title.x = element_textbox_simple(
      halign = 1, colour = text_col,
      margin = margin(t = 3)
    )
  ) +
  canvas(
    width = 6, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-08-11", paste0("20260811", ".png"))
)
