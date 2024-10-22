# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(geofacet)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-10-22")
cia_factbook <- tuesdata$cia_factbook


# Load fonts --------------------------------------------------------------

font_add_google("Libre Franklin", "libre")
font_add_google("Domine", "domine")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#E5E5E5"
bg_col2 <- "#bbbbbb"
text_col <- "#14213D"
highlight_col <- "#5AAA95"
missing_col <- "#6B2D5C"

body_font <- "libre"
title_font <- "domine"


# Data wrangling ----------------------------------------------------------

internet_data <- cia_factbook |>
  mutate(
    internet_perc = 100 * (internet_users / population),
    non_internet_perc = 100 - internet_perc,
    missing_perc = 100 - (internet_perc + non_internet_perc)
  ) |>
  select(country, internet_perc, non_internet_perc, missing_perc) |>
  rename(name = country) |>
  pivot_longer(-name, names_to = "internet", values_to = "perc") |>
  mutate(
    name = case_when(
      name == "Russia" ~ "Russian Federation",
      name == "Antigua and Barbuda" ~ "Antigua & Barbuda",
      name == "Bahamas, The" ~ "Bahamas",
      name == "Bosnia and Herzegovina" ~ "Bosnia & Herzegovina",
      name == "Brunei" ~ "Brunei Darussalam",
      name == "Congo, Republic of the" ~ "Congo",
      name == "Congo, Democratic Republic of the" ~ "Congo (Democratic Republic of the)",
      name == "Cote d'Ivoire" ~ "Côte d'Ivoire",
      name == "Gambia, The" ~ "Gambia",
      name == "Iran" ~ "Iran (Islamic Republic of)",
      name == "Korea, North" ~ "North Korea",
      name == "Korea, South" ~ "South Korea",
      name == "Laos" ~ "Lao People's Democratic Republic",
      name == "Micronesia, Federated States of" ~ "Micronesia (Federated States of)",
      name == "Moldova" ~ "Moldova (Republic of)",
      name == "Burma" ~ "Myanmar",
      name == "Russia" ~ "Russian Federation",
      name == "Saint Kitts and Nevis" ~ "St. Kitts & Nevis",
      name == "Saint Lucia" ~ "St. Lucia",
      name == "Saint Vincent and the Grenadines" ~ "St. Vincent & the Grenadines",
      name == "Trinidad and Tobago" ~ "Trinidad & Tobago",
      name == "United Kingdom" ~ "Great Britain and Northern Ireland",
      name == "United States" ~ "United States of America",
      name == "Vietnam" ~ "Viet Nam",
      TRUE ~ name
    )
  )

# join world_countries_grid1
plot_data <- world_countries_grid1 |>
  left_join(internet_data, by = "name") |>
  as_tibble() |>
  select(code_alpha3, internet, perc) |>
  filter(
    !((internet == "internet_perc" | internet == "non_internet_perc") & is.na(perc))
  ) |>
  mutate(
    perc = replace_na(perc, 100)
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-10-22", "recording"),
  device = "png",
  width = 5,
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
  twitter = NA
)
title <- "Internet users around the world"
st <- glue("The World Factbook provides basic intelligence on the history, people,
government, economy, energy, geography, environment, communications,
transportation, military, terrorism, and transnational issues for 265
world entities. This chart shows the <span style='color:{highlight_col}'>**percentage of the total population who are 
internet users**</span> for each country. For some countries, <span style='color:{missing_col}'>**data is missing**</span>.")
st <- paste0(
  st, "<br><br>**Data**: CIA World Factbook<br>**Graphic**: ", social
)

# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  geom_col(
    mapping = aes(x = 1, y = perc, fill = internet),
    show.legend = FALSE
  ) +
  facet_geo(~code_alpha3, grid = "world_countries_grid1", label = "code_alpha3") +
  labs(title = title, subtitle = st) +
  scale_fill_manual(
    values = c(
      "internet_perc" = highlight_col, "non_internet_perc" = bg_col2,
      "missing_perc" = missing_col
    )
  ) +
  #xlim(c(0, 2)) +
  coord_polar(theta = "y", direction = -1) +
  theme_void(base_family = body_font, base_size = 24) +
  theme(
    strip.text = element_blank(),
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(0, 5, 0, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.spacing = unit(0, "lines"),
    # Text
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      size = rel(2),
      family = title_font,
      maxwidth = 0.9
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 50, t = 5),
      lineheight = 0.45,
      family = body_font,
      maxwidth = 1
    )
  )

record_polaroid()


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-10-22", paste0("20241022", ".png")),
  bg = bg_col,
  width = 5,
  height = 6
)

gg_playback(
  name = file.path("2024", "2024-10-22", paste0("20241022", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
