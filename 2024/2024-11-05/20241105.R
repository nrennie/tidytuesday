# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(geofacet)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-11-05")
democracy_data <- tuesdata$democracy_data


# Load fonts --------------------------------------------------------------

font_add_google("Carter One", "carter")
font_add_google("Source Sans 3", "source")
showtext_auto()

body_font <- "source"
title_font <- "carter"


# Data wrangling ----------------------------------------------------------

plot_data <- democracy_data |>
  select(country_name, country_code, year, regime_category) |>
  mutate(
    category = case_when(
      regime_category %in% c(
        "Royal dictatorship", "Military dictatorship", "Civilian dictatorship",
        "Presidential democracy", "Mixed democratic", "Parliamentary democracy"
      ) ~ regime_category,
      TRUE ~ "Other"
    )
  ) |>
  mutate(
    category = factor(category, levels = c(
      "Royal dictatorship", "Military dictatorship", "Civilian dictatorship",
      "Presidential democracy", "Mixed democratic", "Parliamentary democracy",
      "Other"
    ))
  ) |> 
  mutate(
    country_code = case_when(
      country_code == "GER" ~ "DEU",
      country_code == "ZAR" ~ "COD",
      country_code == "NUR" ~ "NRU",
      country_code == "ROM" ~ "ROU",
      TRUE ~ country_code
    )
  )


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "#2d004b"
col_palette <- c("#b35806", "#f1a340", "#fee0b6", "#d8daeb", "#998ec3", "#542788", "#777777")
names(col_palette) <- levels(plot_data$category)
highlight_col <- col_palette[2]


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-11-05", "recording"),
  device = "png",
  width = 7,
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
title <- "Dictatorship or democracy?"
st <- glue("Data from *Regime types and regime change: A new dataset on democracy, coups, and political institutions* by C. Bjørnskov and M. Rode 
(2020) shows the range of systems
           countries around the world have had between 1950 and 2020. Countries have
            <span style='color:{col_palette[[1]]}'>**{names(col_palette[1])}**</span>,
  <span style='color:{col_palette[[2]]}'>**{names(col_palette[2])}**</span>,
  <span style='color:{col_palette[[3]]}'>**{names(col_palette[3])}**</span>,
  <span style='color:{col_palette[[4]]}'>**{names(col_palette[4])}**</span>,
  <span style='color:{col_palette[[5]]}'>**{names(col_palette[5])}**</span>,
  <span style='color:{col_palette[[6]]}'>**{names(col_palette[6])}**</span>, and
  <span style='color:{col_palette[[7]]}'>**{names(col_palette[7])}**</span> systems.
           ")
cap <- paste0(
  "**Data**: {democracyData}<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  geom_raster(
    mapping = aes(x = year, y = "1", fill = category)
  ) +
  geom_text(
    mapping = aes(x = 1985, y = "1", label = country_code),
    size = 6,
    colour = bg_col,
    family = title_font
  ) +
  facet_geo(~country_code, grid = "world_countries_grid1", label = "code_alpha3") +
  scale_fill_manual(
    values = col_palette
  ) +
  scale_x_continuous(limits = c(1949, 2021)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_family = body_font, base_size = 28) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      size = rel(1.7)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = -25),
      lineheight = 0.5,
      family = body_font
    )
  )
record_polaroid()


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-11-05", paste0("20241105", ".png")),
  bg = bg_col,
  width = 7,
  height = 6
)

gg_playback(
  name = file.path("2024", "2024-11-05", paste0("20241105", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
