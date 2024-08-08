# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggsankey)
library(countrycode)


# Load data ---------------------------------------------------------------

olympics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-06/olympics.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Libre Franklin", "libre")
font_add_google("Domine", "domine")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
col_palette <- c("#ee334e", "#000000", "#0081c8", "#fcb131", "#00a651", "grey70")
highlight_col <- col_palette[1]

body_font <- "libre"
title_font <- "domine"


# Data wrangling ----------------------------------------------------------

cities <- olympics |>
  filter(season == "Summer") |>
  select(year, city) |>
  distinct() |>
  arrange(year) |>
  filter(
    !(year == 1956 & city == "Stockholm")
  ) |>
  mutate(
    city = if_else(
      year == 1956, "Melbourne & Stockholme", city
    )
  )

regions <- olympics |>
  select(noc) |>
  distinct() |>
  arrange(noc) |>
  mutate(
    country = countrycode(noc, origin = "ioc", destination = "country.name"),
    continent = countrycode(noc, origin = "ioc", destination = "continent")
  ) |>
  mutate(
    continent = case_when(
      noc == "AHO" ~ "Americas", # Netherlands Antilles
      noc == "ANZ" ~ "Oceania", # Australasia
      noc == "BOH" ~ "Europe", # Bohemia
      noc == "CRT" ~ "Europe", # Crete
      noc == "EUN" ~ "Europe", # United Team of Germany
      noc == "FRG" ~ "Europe", # West Germany
      noc == "GDR" ~ "Europe", # East Germany
      noc == "IOA" ~ "other", # Independent Olympic Athletes
      noc == "KOS" ~ "Europe", # Kosovo
      noc == "LIB" ~ "Asia", # Lebanon
      noc == "MAL" ~ "Asia", # Malaysia
      noc == "NBO" ~ "Asia", # North Borneo
      noc == "NFL" ~ "Americas", # Newfoundland
      noc == "RHO" ~ "Africa", # Rhodesia
      noc == "ROT" ~ "other", # Refugee Olympic Team
      noc == "SAA" ~ "Europe", # Saar
      noc == "SCG" ~ "Europe", # Serbia & Montenegro
      noc == "TCH" ~ "Europe", # Czechoslovakia
      noc == "UAR" ~ "Africa", # United Arab Republic
      noc == "UNK" ~ "other", # Unknown
      noc == "URS" ~ "Europe", # Soviet Union
      noc == "VNM" ~ "Asia", # South Vietnam
      noc == "WIF" ~ "Americas", # West Indies Federation
      noc == "YAR" ~ "Asia", # North Yemen
      noc == "YMD" ~ "Asia", # South Yemen
      noc == "YUG" ~ "Europe", # Yugoslavia
      TRUE ~ continent
    )
  )

plot_data <- olympics |>
  filter(season == "Summer") |>
  drop_na(medal) |>
  left_join(regions, by = "noc") |>
  count(continent, year) |>
  tibble::add_row(continent = "Africa", year = 1916, n = 0) |>
  tibble::add_row(continent = "Africa", year = 1940, n = 0) |>
  tibble::add_row(continent = "Africa", year = 1944, n = 0) |>
  complete(continent, year, fill = list(n = 0))

continent_levels <- plot_data |>
  group_by(continent) |>
  summarise(n = sum(n)) |>
  arrange(desc(n)) |>
  pull(continent)

plot_data$continent <- factor(plot_data$continent, levels = rev(continent_levels))
names(col_palette) <- continent_levels

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-08-05", "recording"),
  device = "png",
  width = 8,
  height = 4,
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
title <- "A Visual History of Which Continents Have Dominated the Summer Olympics"
st <- glue("Athletes from 
           <span style='color:{col_palette[[1]]}'>{names(col_palette)[[1]]}</span>, 
  <span style='color:{col_palette[[2]]}'>{names(col_palette)[[2]]}</span>,
  <span style='color:{col_palette[[3]]}'>{names(col_palette)[[3]]}</span>,
  <span style='color:{col_palette[[4]]}'>{names(col_palette)[[4]]}</span>, and 
  <span style='color:{col_palette[[5]]}'>{names(col_palette)[[5]]}</span> have won medals in the 
           Summer Olympics, held every four years. Unknown countries, independent olympic athletes, and the refugee olympic team are included in <span style='color:{col_palette[[6]]}'>{names(col_palette)[[6]]}</span> athletes.")
cap <- paste0(
  st, 
  "<br><br>**Data**: Sports Reference | **Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(
    x = year,
    value = n,
    node = factor(continent, levels = rev(continent_levels)),
    next_node = factor(continent, levels = rev(continent_levels)),
    fill = factor(continent, levels = rev(continent_levels))
  )
) +
  geom_segment(
    data = cities,
    mapping = aes(
      x = year, xend = year,
      y = 1000, yend = -1500
    ),
    inherit.aes = FALSE,
    colour = alpha(text_col, 0.2),
    linewidth = 0.3
  ) +
  geom_text(
    data = cities,
    mapping = aes(
      x = year, y = -1500,
      label = paste(year, city)
    ),
    inherit.aes = FALSE,
    colour = alpha(text_col, 0.6),
    family = body_font,
    size = 5,
    hjust = 0,
    vjust = 1.5,
    angle = 90
  ) +
  annotate(
    "text",
    x = 1942, y = -750,
    label = str_wrap("No Olympic Games held during World War II", 8),
    colour = text_col,
    family = body_font,
    size = 5,
    lineheight = 0.5
  ) +
  annotate(
    "text",
    x = 1916, y = -750,
    label = str_wrap("World War I", 8),
    colour = text_col,
    family = body_font,
    size = 5,
    lineheight = 0.5
  ) +
  geom_sankey_bump(
    space = 1,
    color = "transparent",
    smooth = 5,
    alpha = 0.7
  ) +
  scale_fill_manual(
    values = rev(col_palette)
  ) +
  labs(
    title = title,
    subtitle = cap
  ) +
  theme_void(base_size = 18, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      size = rel(2),
      family = title_font,
      maxwidth = 0.7
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.7
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-08-06", paste0("20240806", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
