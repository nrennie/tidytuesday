# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(geofacet)
library(MetBrewer)
library(ggiraph)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-04-21")
financing_schemes <- tuesdata$financing_schemes
health_spending <- tuesdata$health_spending
spending_purpose <- tuesdata$spending_purpose


# Load fonts --------------------------------------------------------------

font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

text_col <- "#F2F4F8"
bg_col <- "#151C28"


# Data wrangling ----------------------------------------------------------

all_iso3 <- geofacet::world_countries_grid1 |> 
  as_tibble() |> 
  pull(code_alpha3)

spending_data <- spending_purpose |> 
  filter(indicator_code == "hc6_che") |> 
  select(iso3_code, year, value) |> 
  filter(!(iso3_code %in% c("PSE", "LIE"))) |> 
  mutate(iso3_code = factor(iso3_code, levels = all_iso3)) |> 
  complete(iso3_code, year) |> 
  rename(code_alpha3 = iso3_code) |> 
  mutate(code_alpha3 = as.character(code_alpha3))

plot_data <- geofacet::world_countries_grid1 |> 
  as_tibble() |> 
  left_join(spending_data, by = "code_alpha3", relationship = "one-to-many") |> 
  select(code_alpha3, name, year, value)

label_data <- plot_data |> 
  drop_na() |> 
  group_by(code_alpha3) |>
  slice_max(year) |> 
  mutate(
    label = glue("<b>{name}</b><br>{round(value, 1)}% in {year}")
  ) |> 
  select(code_alpha3, label)

final_data <- plot_data |> 
  left_join(label_data, by = c("code_alpha3")) |> 
  mutate(label = if_else(
    is.na(label), glue("<b>{name}</b><br>No data available"), label
  ))


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- glue("<span style='font-size:11pt;'>**African countries spend more on preventive care**</span><br>")
st <- glue("<span style='font-size:8pt;'>Countries in Africa typically spent a <span style='color:{met.brewer(\"Tam\")[2]};'>**higher percentage**</span> of their total healthcare spending on preventive care between 2016 and 2023. European and North American countries typically spent a <span style='color:{met.brewer(\"Tam\")[6]};'>**lower percentage**</span>. For many countries, <span style='color:#31415E;'>**no data**</span> is available.</span><br>")
cap <- source_caption(source = "WHO Global Health Expenditure Database", graphic = social)
txt <- paste0(
  title, st, "<br>", cap
)


# Plot --------------------------------------------------------------------

g <- ggplot(data = final_data) +
  geom_raster(
    mapping = aes(x = year, y = 1, fill = value)
  ) +
  geom_rect_interactive(
    mapping = aes(xmin = 2016 - 0.5, xmax = 2023 + 0.5,
                  ymin = 0.5, ymax = 1.5, 
                  data_id = code_alpha3, tooltip = label),
    fill = "transparent",
    colour = "transparent"
  ) +
  facet_geo(~ code_alpha3, grid = "world_countries_grid1") +
  labs(tag = txt) +
  scale_fill_met_c("Tam", na.value = "#2A3850", direction = -1) +
  theme_void(base_size = 6, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font,
      maxwidth = 0.4
    ),
    plot.tag.position = c(0.01, 0.14),
    strip.text = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.spacing = unit(0.05, "lines")
  )

# Static
g +
  canvas(
    width = 7, height = 5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p

# Interactive
girafe(
  ggobj = g,
  width_svg = 7,
  height_svg = 5,
  options = list(
    opts_toolbar(saveaspng = FALSE),
    opts_hover(css = "fill:#F2F4F8;"),
    opts_tooltip(
      delay_mouseout = 1000,
      css = "padding: 5pt;
             font-family: Nunito;
             font-size: 1em;
             color: #ffffff;
             background-color: #31415E;"
    )
  )
)


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-04-21", paste0("20260421_static", ".png"))
)
