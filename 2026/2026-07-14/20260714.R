# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(geofacet)
library(rcartocolor)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-07-14")
many_penguins <- tuesdata$many_penguins


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

# Grid layout
all_species <- many_penguins |>
  select(from = genus, to = species) |>
  distinct() |>
  mutate(level = "species")
all_genus <- tibble(
  from = rep("Spheniscidae", length(unique(all_species$from))),
  to = unique(all_species$from)
) |>
  mutate(level = "genus")
all_penguins <- rbind(all_genus, all_species)
dat1 <- all_penguins |>
  arrange(level, to) |>
  group_by(level) |>
  mutate(x_grid = row_number()) |>
  ungroup()
dat2 <- dat1 |>
  filter(level == "species") |>
  group_by(from) |>
  summarise(x_grid = round(median(x_grid))) |>
  ungroup()
grid_data <- dat1 |>
  left_join(dat2, by = c("to" = "from")) |>
  mutate(
    x_grid = if_else(
      is.na(x_grid.y), x_grid.x, x_grid.y
    )
  ) |>
  select(name = to, level, col = x_grid) |>
  add_row(
    name = "Spheniscidae", level = "family", col = round(median(dat2$x_grid))
  ) |>
  mutate(
    row = case_when(
      level == "family" ~ 1,
      level == "genus" ~ 2,
      TRUE ~ 3
    )
  ) |>
  arrange(desc(row)) |>
  mutate(code = name) |>
  select(name, code, row, col)

# Scatter data
family_penguins <- many_penguins |>
  mutate(name = "Spheniscidae") |>
  select(name, sex, beak.width, tail.length)
genus_penguins <- many_penguins |>
  select(name = genus, sex, beak.width, tail.length)
species_penguins <- many_penguins |>
  select(name = species, sex, beak.width, tail.length)
plot_data <- rbind(family_penguins, rbind(genus_penguins, species_penguins)) |>
  drop_na()

# Colours
col_palette <- rcartocolor::carto_pal(7, "Safe")
names(col_palette) <- c(all_genus$to, "Spheniscidae")

# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Penguins in the genus 'Pygoscelis' tend to have both wider beaks and longer tails."
st <- "The AVONET database contains comprehensive functional trait data for all birds, including six ecological variables, eleven continuous morphological traits, and information on range size and location. Raw morphological measurements are available from 90,020 individuals of 11,009 extant bird species sampled from 181 countries."
cap <- source_caption(source = "AVONET database (Tobias et al. 2022)", graphic = social, sep = " | ")


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = beak.width, y = tail.length)
) +
  geom_rect(
    data = plot_data |> group_by(name) |> slice_head(n = 1),
    mapping = aes(
      xmin = 0, xmax = 20,
      ymin = 0, ymax = 170,
      fill = stringr::str_extract(name, '^[^ ]+')
    ),
    alpha = 0.2
  ) +
  geom_point(
    pch = 21, colour = bg_col, alpha = 0.9, size = 2,
    fill = text_col
  ) +
  facet_geo(~name, grid = grid_data) +
  labs(
    x = "**Beak width**: Width of the beak at the anterior edge of the nostrils (mm)", ,
    y = "**Tail length**: Distance between the tip of the longest rectrix and the point at which<br>the two central rectrices protrude from the skin, typically measured using a ruler<br>inserted between the two central rectrices (mm).",
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_x_continuous(
    limits = c(0, 20),
    breaks = c(0, 10, 20),
    labels = c("", 10, 20)
  ) +
  scale_y_continuous(
    limits = c(0, 170),
    breaks = c(0, 85, 170)
  ) +
  scale_fill_manual(
    values = col_palette
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_bw(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.title.x = element_textbox_simple(
      hjust = 1, halign = 1, colour = "grey30",
      valign = 0,
      vjust = 0,
      margin = margin(t = 2)
    ),
    axis.title.y = element_textbox_simple(
      hjust = 0, halign = 0, colour = "grey30",
      width = unit(0, "cm"),
      valign = 1, vjust = 1,
      margin = margin(r = -460)
    ),
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
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(b = 3, t = 3),
      size = rel(0.6),
      valign = 0,
      vjust = 0
    ),
    strip.clip = "off",
    panel.spacing.y = unit(1.5, "lines"),
    panel.grid.minor = element_blank()
  ) +
  canvas(
    width = 12, height = 5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-07-14", paste0("20260714_raw", ".svg"))
)

# Further annotations made with Inkscape
# Cowplot and geofacet are not playing nicely together :(




