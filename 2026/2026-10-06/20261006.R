# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


# Functions ---------------------------------------------------------------

avocado_data <- function(data, aspect = 1.7, shoulder = 0.35, n = 400) {
  q <- log(0.5) / log(shoulder)
  profile <- function(t) sqrt(sin(pi * t^q))
  integral <- integrate(profile, 0, 1)$value
  build_shapes <- function(w, h, y_centre, r) {
    t <- seq(0, 1, length.out = n)
    x <- w * profile(t)
    theta <- seq(0, 2 * pi, length.out = n)
    bind_rows(
      tibble(
        part = "avocado",
        x = c(x, -rev(x)),
        y = c(h * t, rev(h * t))
      ),
      tibble(
        part = "stone",
        x = r * cos(theta),
        y = y_centre + r * sin(theta)
      )
    )
  }
  output <- data |>
    mutate(
      id = row_number(),
      w = sqrt(area / (2 * aspect * integral)),
      h = aspect * w,
      y_centre = 0.4 * h,
      r = sqrt(inner_area / pi),
      shapes = pmap(list(w, h, y_centre, r), build_shapes)
    ) |>
    unnest(shapes) |>
    mutate(group = paste(id, part)) |>
    select(category, id, part, group, x, y)
  return(output)
}

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-10-06")
avocado_oil_bottles <- tuesdata$avocado_oil_bottles
avocado_oil_processed_foods <- tuesdata$avocado_oil_processed_foods


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
highlight_col <- "#c9d96a"


# Data wrangling ----------------------------------------------------------

avo_data <- avocado_oil_processed_foods |>
  filter(oil_type == "avocado") |>
  mutate(
    category = str_replace(category, "_", " "),
    category = str_to_sentence(category)
  ) |>
  count(category, authentic) |>
  group_by(category) |>
  mutate(tot = sum(n)) |>
  ungroup() |>
  mutate(
    n = if_else(authentic, n, tot),
    authentic = if_else(authentic, "inner_area", "area")
  ) |>
  select(-tot) |>
  pivot_wider(
    names_from = authentic, values_from = n,
    values_fill = 0
  ) |>
  mutate(
    inner_area = inner_area / area,
    area = 1
  ) |> 
  arrange(desc(inner_area)) |> 
  mutate(category = factor(category, levels = category)) |> 
  mutate(
    label = if_else(
      inner_area != 0, 
      paste0("Only **", round(100 * inner_area), "%** of tested ", 
             str_to_lower(category),
             " passed authenticity testing."),
      "**None** of the tested salad dressings passed authenticity testing."
    )
  )

plot_data <- avocado_data(avo_data, aspect = 2.5, shoulder = 0.35, n = 400)

txt_data <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  category = c("Mayonnaise", "Chips", "Salad dressing"),
  label = c("")
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Holy Guacamole! Your avocado oil probably isn't real."
st <- "A 2026 study by researchers at UC Davis examined processed foods made with avocado oil, which revealed widespread fraud in the avocado oil market. 89% of chips, mayonnaise, and salad dressings labelled as '*made with avocado oil*' failed authenticity testing."
cap <- paste0(st, "<br><br>", source_caption(source = "UC Davis Department of Food Science and Technology", graphic = social))


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = x, y = y, group = group, fill = part, colour = part)
) +
  geom_polygon(linewidth = 1) +
  geom_textbox(
    data = avo_data,
    mapping = aes(x = 0, y = -0.2, label = label),
    inherit.aes = FALSE,
    family = body_font,
    hjust = 0.5,
    halign = 0.5,
    fill = "transparent",
    colour = text_col,
    box.colour = "transparent"
  ) +
  scale_fill_manual(values = c(avocado = "#c9d96a", stone = "#6b4423")) +
  scale_colour_manual(values = c(avocado = "#2d4a1e", stone = "#6b4423")) +
  facet_wrap(~category) +
  labs(
    title = title,
    subtitle = cap
  ) +
  coord_fixed(clip = "off") +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(-10, 10, 5, 10),
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
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(1),
      hjust = 0.5,
      halign = 0.5
    )
  ) +
  canvas(
    width = 7, height = 5.5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-10-06", paste0("20261006", ".png"))
)
