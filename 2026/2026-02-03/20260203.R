# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(grid)
library(patchwork)
library(scales)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-02-03")
edible_plants <- tuesdata$edible_plants


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "#151C28"
highlight_col <- "#7F055F"


# Data wrangling ----------------------------------------------------------

plot_data <- edible_plants |>
  select(cultivation, preferred_ph_lower, preferred_ph_upper) |>
  mutate(cultivation = case_when(
    cultivation == "Brassicas" ~ "Brassica",
    TRUE ~ cultivation
  ))

ph_ranges <- plot_data |>
  group_by(cultivation) |>
  summarise(
    min_ph = min(preferred_ph_lower),
    max_ph = max(preferred_ph_upper)
  ) |>
  mutate(range = max_ph - min_ph) |> 
  arrange(range)

plot_data$cultivation <- factor(plot_data$cultivation, levels = ph_ranges$cultivation)
ph_ranges$cultivation <- factor(ph_ranges$cultivation, levels = ph_ranges$cultivation)

bars_data <- tibble(
  start = rep(seq(min(ph_ranges$min_ph), max(ph_ranges$max_ph), by = 0.1), 
              each = length(as.character(ph_ranges$cultivation)))
) |> 
  mutate(
    end = start + 0.1,
    cultivation = rep(as.character(ph_ranges$cultivation),
                      times = length(seq(min(ph_ranges$min_ph), max(ph_ranges$max_ph), by = 0.1)))
  ) |> 
  rowwise() |> 
  mutate(
    count = sum(plot_data$preferred_ph_lower[plot_data$cultivation == cultivation] <= start & plot_data$preferred_ph_upper[plot_data$cultivation == cultivation] > end)
  ) |> 
  ungroup() |> 
  mutate(
    rescaled = rescale(count, to = c(0, 1)),
    alpha = 1 - rescaled,
    cultivation = factor(cultivation, levels = ph_ranges$cultivation)
  ) 


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Most edible plants require neutral soil"
st <- "Based on information for 146 edible plant species in the Edible Plant Database, a European Citizen Science project on growing food, soil moisture sensing and land monitoring."
cap <- source_caption(source = "Edible Plants Database", graphic = social)


# Plot --------------------------------------------------------------------

squashed_rainbow <- linearGradient(
  c("red", "orange", "yellow", "green", "blue", "darkblue", "purple"),
  x1 = 0, y1 = 0, x2 = 1, y2 = 0, group = FALSE
)

p1 <- ggplot() +
  annotate("rect",
    xmin = 0, xmax = 14, ymin = 0, ymax = 1,
    fill = squashed_rainbow
  ) +
  geom_textbox(
    data = data.frame(
      x = c(0, 7, 14),
      label = c("Acidic", "Neutral", "Alkaline"),
      hjust = c(0, 0.5, 1)
    ),
    mapping = aes(x = x, y = 1.5, hjust = hjust, halign = hjust, label = label),
    size = 3.5,
    box.padding = unit(c(5.5, 0, 5.5, 0), "pt"),
    fill = "transparent",
    box.colour = "transparent",
    colour = text_col,
    fontface = "bold",
    family = body_font
  ) +
  scale_y_continuous(limits = c(0, 2)) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_col, colour = bg_col))

p2 <- ggplot() +
  annotate("rect",
    xmin = 0, xmax = 14, ymin = 0, ymax = 1,
    fill = squashed_rainbow
  ) +
  geom_rect(
    data = ph_ranges,
    mapping = aes(
      xmin = 0,
      xmax = min_ph,
      ymin = -0.03,
      ymax = 1.03
    ),
    fill = "#F2F4F8"
  ) +
  geom_rect(
    data = ph_ranges,
    mapping = aes(
      xmin = max_ph,
      xmax = 14,
      ymin = -0.03,
      ymax = 1.03
    ),
    fill = "#F2F4F8"
  ) +
  facet_wrap(~cultivation, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 14, 1)) +
  scale_y_continuous(limits = c(-0.03, 1.03)) +
  labs(x = "Preferred pH range") +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 0),
      size = rel(0.9)
    ),
    panel.spacing = unit(0, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(hjust = 1),
    axis.text.y = element_blank()
  )

p3 <- p2 +
  geom_rect(
    data = bars_data,
    mapping = aes(
      xmin = start,
      xmax = end,
      ymin = -0.03,
      ymax = 1.03,
      alpha = alpha
    ),
    fill = "#F2F4F8"
  ) +
  geom_rect(
    data = ph_ranges,
    mapping = aes(
      xmin = min_ph,
      xmax = max_ph,
      ymin = 0,
      ymax = 1
    ),
    fill = "transparent",
    colour = text_col,
    linewidth = 0.2
  ) +
  scale_alpha_identity() 

p1 + p2 + 
  plot_layout(ncol = 1, heights = c(1, 12)) +
  plot_annotation(
    title = title, subtitle = st,
    caption = cap,
    theme = theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
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
        margin = margin(b = 0, t = 5),
        family = body_font
      )
    )
  ) +
  canvas(
    width = 5, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


p1 + p3 + 
  plot_layout(ncol = 1, heights = c(1, 12)) +
  plot_annotation(
    title = title, subtitle = st,
    caption = cap,
    theme = theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
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
        margin = margin(b = 0, t = 5),
        family = body_font
      )
    )
  ) +
  canvas(
    width = 5, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p_alt


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-02-03", paste0("20260203", ".png"))
)

save_ggplot(
  plot = p_alt,
  file = file.path("2026", "2026-02-03", paste0("20260203_alt", ".png"))
)
