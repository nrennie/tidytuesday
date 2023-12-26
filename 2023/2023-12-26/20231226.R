# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(rcartocolor)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-12-26")
cran_20221122 <- tuesdata$cran_20221122
external_calls <- tuesdata$external_calls
internal_calls <- tuesdata$internal_calls


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#FAFAFA"
text_col <- "grey10"
col_palette <- carto_pal(6, "Bold")[1:5]
highlight_col <- col_palette[3]

body_font <- "roboto"
title_font <- "robotoslab"

# Data wrangling ----------------------------------------------------------

# Function to get number of dependencies
number_deps <- function(x) {
  if (is.na(x)) {
    return(0)
  } else {
    return(nrow(stringr::str_locate_all(x, ",")[[1]]) + 1)
  }
}

# count dependencies
cran_pkgs <- cran_20221122 |>
  select(package, date, depends, imports, suggests, enhances, linking_to) |>
  mutate(date = year(date)) |>
  filter(date >= 2010) |>
  pivot_longer(-c(package, date), values_to = "pkgs", names_to = "type") |>
  mutate(pkgs = purrr::map_vec(.x = pkgs, .f = ~ number_deps(.x)))

# format
plot_data <- cran_pkgs |>
  group_by(date, type) |>
  summarise(count = sum(pkgs)) |>
  mutate(
    date.count = sum(count),
    prop = count / sum(count)
  ) |>
  ungroup() |>
  mutate(date = factor(date)) |> 
  mutate(type = factor(type,
                       levels = c(
                         "depends", "imports", "suggests", "enhances", "linking_to"
                       )))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-12-26", "recording"),
  device = "png",
  width = 7,
  height = 5,
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
title <- "R Package Dependencies"
st <- glue::glue("For R packages on CRAN, as of 22 November 2022, almost half 
                 of packages were released in the previous two years. For 
                 packages released between 2010 and 2022, there are trends in whether 
                 dependencies are listed in the <span style='color:{col_palette[1]}'>depends</span>, 
                 <span style='color:{col_palette[2]}'>imports</span>, 
                 <span style='color:{col_palette[3]}'>suggests</span>, 
                 <span style='color:{col_palette[4]}'>enhances</span>, or 
                 <span style='color:{col_palette[5]}'>linking_to</span> fields 
                 of the package DESCRIPTION file. A decreasing percentage of 
                 dependencies are listed in the *depends* and *enhances* fields, with 
                 an increasing percentage seen in *suggests*.")
cap <- paste0(
  "**Data**: Historical Trends in R Package Structure and Interdependency on CRAN. Padgham & Ross. 2022.<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_col(data = plot_data,
           mapping = aes(
             x = date,
             y = prop,
             width = date.count, fill = type
           ),
           position = "fill", colour = bg_col,
           linewidth = 0.2) +
  geom_text(data = data.frame(date = factor(2016:2022),
                              y = 1.05),
            mapping = aes(x = date, y = y, label = date),
            family = body_font,
            colour = text_col,
            size = 6) +
  facet_grid(~date, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = col_palette) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  theme_void(base_size = 24, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.spacing = unit(0.03, "lines"),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      face = "bold",
      size = 50,
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    strip.text = element_blank()
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-12-26", paste0("20231226", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
