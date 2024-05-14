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

tuesdata <- tidytuesdayR::tt_load("2024-04-30")
wwbi_data <- tuesdata$wwbi_data
wwbi_series <- tuesdata$wwbi_series
wwbi_country <- tuesdata$wwbi_country


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
highlight_col <- "#4EA699"
fem_col <- "#8D80AD"

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------

choose_ind <- wwbi_series |>
  filter(indicator_name %in% c(
    "Females, as a share of public paid employees",
    "Number of public paid employees"
  ))

choose_region <- wwbi_country |>
  select(country_code, short_name, x2_alpha_code) |>
  filter(x2_alpha_code %in% geofacet::europe_countries_grid1$code)

plot_data <- wwbi_data |>
  filter(indicator_code %in% choose_ind$indicator_code) |>
  filter(country_code %in% choose_region$country_code) |>
  group_by(country_code) |>
  slice_max(year) |>
  ungroup() |>
  left_join(choose_region, by = "country_code") |>
  select(x2_alpha_code, short_name, indicator_code, value) |>
  pivot_wider(names_from = indicator_code, values_from = value) |>
  mutate(
    Males = 1 - BI.PWK.PUBS.FE.ZS
  ) |>
  rename(Females = BI.PWK.PUBS.FE.ZS) |>
  pivot_longer(
    cols = c(Males, Females),
    names_to = "Gender",
    values_to = "Share"
  ) |> 
  mutate(
    New_Gender = case_when(
      is.na(Share) ~ "Missing",
      TRUE ~ Gender
    ),
    Share = case_when(
      is.na(Share) & Gender == "Females" ~ 1,
      is.na(Share) & Gender == "Males" ~ NA,
      TRUE ~ Share
    )
  ) |> 
  filter(x2_alpha_code != "TR")

new_grid <- europe_countries_grid1 |> 
  left_join(choose_region[,2:3], by = c("code" = "x2_alpha_code")) |> 
  as_tibble() |> 
  select(-name) |> 
  rename(name = short_name) |> 
  mutate(name = str_wrap(name, 10))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-04-30", "recording"),
  device = "png",
  width = 5,
  height = 7.5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Public sector employment in Europe"
st <- glue("The Worldwide Bureaucracy Indicators (WWBI) database is a unique 
           cross-national dataset on public sector employment and wages that 
           aims to fill an information gap. This infographic shows the share of <span style='color:{highlight_col};'>**males**</span> and 
           <span style='color:{fem_col};'>**females**</span> in public paid employment, with the size of each 
           pie chart indicating the total number of public paid employees. Grey 
           pie charts indicate countries where the share of male and female employees 
           is unknown.")
cap <- paste0(
  "**Data**: World Bank<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(plot_data) +
  geom_arc_bar(
    mapping = aes(
      x0 = 0, y0 = 0, r0 = 0, r = BI.PWK.PUBS.NO,
      amount = Share,
      fill = New_Gender
    ),
    stat = "pie",
    colour = NA
  ) +
  scale_fill_manual(
    values = c(
      "Males" = highlight_col,
      "Females" = fem_col,
      "Missing" = "grey50"
    )
  ) +
  facet_geo(~x2_alpha_code, grid = new_grid, label = "name") +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_fixed(expand = FALSE) +
  theme_void(base_size = 22, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      size = rel(2),
      face = "bold",
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    panel.spacing = unit(0.6, "lines"),
    strip.text.x = element_text(
      lineheight = 0.4,
      vjust = 1,
      size = rel(0.8),
      margin = margin(l = 5, r = 5)
    )
  )

record_polaroid()


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-04-30", paste0("20240430", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
