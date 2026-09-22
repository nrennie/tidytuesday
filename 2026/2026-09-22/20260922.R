# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-09-22")
urban <- tuesdata$urban


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
highlight_col <- "#7F055F"


# Data wrangling ----------------------------------------------------------

plot_data <- urban |>
  filter(countryOrTerritoryName == "United Kingdom of Great Britain and Northern Ireland") |>
  select(year,
    city = cityName,
    green_area = averageShareOfGreenAreaInCityUrbanAreaPct,
  ) |>
  drop_na() |>
  mutate(
    city = str_extract(city, "^.*?(?= \\(|$)"),
    city = str_remove(city, "-Bedworth")
  )

plot_2020 <- plot_data |>
  filter(year == 2020) |>
  arrange(desc(green_area))

plot_data$city <- factor(plot_data$city,
  levels = plot_2020$city
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- ""
st <- ""
cap <- paste0("**Note**: Data for other UK cities is not available.", source_caption(source = "UN Habitat Urban Indicators Database", graphic = social))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = -sqrt(100) / 2, xmax = sqrt(100) / 2,
      ymin = -sqrt(100) / 2, ymax = sqrt(100) / 2
    ),
    fill = "grey70"
  ) +
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = -sqrt(green_area) / 2, xmax = sqrt(green_area) / 2,
      ymin = -sqrt(green_area) / 2, ymax = sqrt(green_area) / 2
    ),
    fill = "#519623"
  ) +
  facet_grid(city ~ year, switch = "y") +
  coord_fixed() +
  theme_minimal(base_size = 10, base_family = body_font) +
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
      hjust = 0.5,
      halign = 0.5,
      size = rel(0.9)
    ),
    strip.text.y.left = element_text(
      face = "bold",
      angle = 0,
      hjust = 1,
      vjust = 0.5,
      size = rel(0.9)
    ),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    panel.spacing = unit(0.01, "lines")
  ) +
  canvas(
    width = 4, height = 8,
    units = "in", bg = bg_col,
    dpi = 300
  ) # -> p

# add perc labels
# add how to read this chart legend/annotations
# Maybe make this a table?

"Green square represents the avreage percentage of ... "

"Grey square represents the total area of the city."

"Dundee has consistently had the lowest percentage of green space."

"Stoke-on-Trent has the highest percentage of green space, improving dramatically since 1990 when it had very little."



# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-09-22", paste0("20260922", ".png"))
)
