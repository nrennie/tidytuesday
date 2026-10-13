# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(countrycode)
library(rnaturalearth)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-10-13")
frogs_and_toads_amphibiaweb <- tuesdata$frogs_and_toads_amphibiaweb


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
highlight_col <- "#426b28"


# Data wrangling ----------------------------------------------------------

frog_data <- frogs_and_toads_amphibiaweb |>
  select(common_name, isocc) |>
  drop_na(common_name) |>
  separate_longer_delim(isocc, ",") |>
  mutate(
    country = countrycode(isocc, "iso2c", "country.name.en"),
    continent = countrycode(isocc, "iso2c", "continent")
  ) |>
  filter(continent == "Europe") |>
  mutate(common_name = str_extract(common_name, "^[^,]+")) |>
  group_by(common_name) |>
  mutate(n = n()) |>
  ungroup() |>
  filter(n >= 10) |> 
  mutate(
    common_name = glue("**{common_name}**<br>*{n} countries*")
  )

top_frogs <- frog_data |>
  group_by(common_name) |>
  slice_head(n = 1) |>
  ungroup() |>
  arrange(n)

eu_map <- ne_countries(
  continent = "Europe",
  scale = "medium"
) |>
  select(name_en, iso_a2, geometry)

# Fix missing ISO2 codes
eu_map |>
  select(name_en, iso_a2) |>
  sf::st_drop_geometry() |>
  filter(iso_a2 == "-99")
eu_map <- eu_map |>
  mutate(
    iso_a2 = case_when(
      name_en == "France" ~ "FR",
      name_en == "Norway" ~ "NO",
      name_en == "Kosovo" ~ "XK",
      TRUE ~ iso_a2
    )
  )

prep_frog_data <- function(frog) {
  f_data <- frog_data |>
    filter(common_name == frog)
  output_data <- eu_map |>
    left_join(
      f_data,
      by = c("iso_a2" = "isocc")
    ) |>
    mutate(fill = if_else(is.na(n), "grey70", highlight_col)) |>
    select(common_name, fill, geometry) |>
    mutate(common_name = frog)
  return(output_data)
}

plot_data <- map(
  .x = unique(frog_data$common_name),
  .f = ~ prep_frog_data(.x)
) |>
  bind_rows() |>
  mutate(
    common_name = factor(
      common_name,
      levels = top_frogs$common_name
    )
  )


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Europe’s Hopping Hotspots: where are all the frogs?"
st <- glue("AmphibiaWeb is an American non-profit website that provides information about amphibians. The map below shows European countries where different species of frogs and toads are <span style='color:{highlight_col};'>**present**</span>, according to the AmphibiaWeb database.")
cap <- source_caption(source = "AmphibiaWeb", graphic = social, note = "Only species present in at least 10 European countries are included.")


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  geom_sf(
    mapping = aes(fill = fill),
    colour = bg_col,
    linewidth = 0.1
  ) +
  scale_fill_identity() +
  facet_wrap(~common_name, ncol = 7) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_sf(
    xlim = c(-30, 50),
    ylim = c(35, 75),
    expand = FALSE
  ) +
  theme_void(base_size = 10, base_family = body_font) +
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
      margin = margin(b = 15, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 20),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      margin = margin(t = 10),
      size = rel(0.9),
      vjust = 0,
      valign = 0,
      minheight = unit(0.5, "in")
    ),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.7, "lines")
  ) +
  canvas(
    width = 7, height = 4.6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-10-13", paste0("20261013", ".png"))
)
