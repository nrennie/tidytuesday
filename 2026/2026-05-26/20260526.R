# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggforce)
library(ggiraph)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-05-26")
energy_cleaned <- tuesdata$energy_cleaned


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#151C28"
text_col <- "#F2F4F8"
rural_col <- "#75DBCD"
urban_col <- "#FBB13C"


# Data wrangling ----------------------------------------------------------

plot_data <- energy_cleaned |>
  select(
    yr, country_name,
    access_non_solid_fuel_rural_pop_pct, access_non_solid_fuel_urban_pop_pct
  ) |>
  drop_na(starts_with("access")) |>
  mutate(
    across(
      starts_with("access"), ~ .x / 100
    )
  ) |> 
  mutate(
    tooltip = glue("<b>{country_name}</b><br>Rural: {round(100 * access_non_solid_fuel_rural_pop_pct)}%<br>Urban: {round(100 * access_non_solid_fuel_urban_pop_pct)}%")
  )


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = rural_col,
  font_colour = text_col,
  font_family = body_font
)
title <- glue("Better access to non-solid fuels in <span style='color:{urban_col};'>urban</span> than <span style='color:{rural_col};'>rural</span> areas")
st <- "The **Sustainable Energy for all (SE4ALL)** initiative, launched in 2010 by the UN Secretary General, established three global objectives to be accomplished by 2030: to ensure universal access to modern energy services, to double the global rate of improvement in global energy efficiency, and to double the share of renewable energy in the global energy mix.<br><br>*Percentage of urban/rural population with access to non-solid fuels like natural gas, LPG, electricity, or ethanol in 2010.*"
cap <- source_caption(source = "SE4ALL Gloabl Database", graphic = social)


# Plot --------------------------------------------------------------------

offset <- 0.03
g <- ggplot(data = plot_data) +
  # arcs
  geom_path_interactive(
    mapping = aes(
      x0 = access_non_solid_fuel_rural_pop_pct / 2,
      y0 = offset, r = access_non_solid_fuel_rural_pop_pct / 2,
      start = -pi / 2, end = pi / 2,
      data_id = country_name,
      tooltip = tooltip
    ),
    stat = ggforce::StatArc,
    colour = rural_col
  ) +
  geom_path_interactive(
    mapping = aes(
      x0 = access_non_solid_fuel_urban_pop_pct / 2,
      y0 = -offset, r = access_non_solid_fuel_urban_pop_pct / 2,
      start = pi / 2, end = 3 * pi / 2,
      data_id = country_name,
      tooltip = tooltip
    ),
    stat = ggforce::StatArc,
    colour = urban_col
  ) +
  # point
  geom_point_interactive(
    mapping = aes(
      x = access_non_solid_fuel_rural_pop_pct,
      y = offset,
      data_id = country_name,
      tooltip = tooltip
    ),
    colour = rural_col
  ) +
  geom_point_interactive(
    mapping = aes(
      x = access_non_solid_fuel_urban_pop_pct,
      y = -offset,
      data_id = country_name,
      tooltip = tooltip
    ),
    colour = urban_col
  ) +
  # axis
  geom_text(
    data = data.frame(x = seq(0.2, 1, 0.2), y = 0),
    mapping = aes(x = x, y = y, 
                  label = paste0(100 * x, "%")),
    family = body_font,
    colour = text_col
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_fixed() +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 10, 5, 5),
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
      size = rel(1.6)
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
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank()
  ) 

g +
  canvas(
    width = 6, height = 8,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-05-26", paste0("20260526", ".png"))
)


# Interactive -------------------------------------------------------------

girafe(
  ggobj = g,
  bg = bg_col,
  width_svg = 6,
  height_svg = 8,
  options = list(
    opts_tooltip(
      delay_mouseover = 500,
      opacity = 0.9,
      css = glue("
        padding: 5pt;
        font-family: {body_font};
        font-size: 1rem;
        background-color: {bg_col};
        color: {text_col};
        border: solid;
        border-color: {text_col};
        border-radius: 5px;
        border-width: 2px")
    ),
    opts_hover(css = "opacity: 1;"),
    opts_hover_inv(css = "opacity: 0.05;"),
    opts_toolbar(hidden = c("saveaspng", "fullscreen")),
    opts_zoom(max = 1)
  )
)

