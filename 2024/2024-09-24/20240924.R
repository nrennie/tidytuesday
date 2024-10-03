# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggsankey)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-09-24")
country_results_df <- tuesdata$country_results_df
individual_results_df <- tuesdata$individual_results_df
timeline_df <- tuesdata$timeline_df


# Load fonts --------------------------------------------------------------

font_add_google("Libre Franklin", "libre")
font_add_google("Domine", "domine")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
col_palette <- c("female" = "#BD4089", "male" = "#8DA7BE")
highlight_col <- col_palette[[1]]

body_font <- "libre"
title_font <- "domine"


# Data wrangling ----------------------------------------------------------

plot_data <- timeline_df |>
  select(year, male_contestant, female_contestant) |>
  pivot_longer(
    cols = -year,
    names_to = "gender", values_to = "n"
  ) |> 
  mutate(
    gender = str_remove_all(gender, "_contestant"),
    year = factor(year),
    n = replace_na(n, 0)
  )

country_data <- timeline_df |> 
  select(year, country) |> 
  distinct() |> 
  mutate(
    country = glue("{country} **{year}**"),
    year = factor(year)
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-09-24", "recording"),
  device = "png",
  width = 8,
  height = 5,
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
title <- "A Visual History of International Mathematical Olympiads"
st <- glue(
  "The International Mathematical Olympiad (IMO) is the World Championship 
  Mathematics Competition for High School students and is held annually in a 
  different country. The first IMO was held in 1959 in Romania, with 7 countries 
  participating. It has gradually expanded to over 100 countries from 5 
  continents. Though the number of contestants has steadily increased over time, 
  the number of <span style='color:{col_palette[[2]]}'>**{names(col_palette)[[2]]}**</span> and 
  <span style='color:{col_palette[[1]]}'>**{names(col_palette)[[1]]}**</span> contestants 
  remains highly imbalanced.")
cap <- glue("**Data**: IMO Official<br>**Graphic**: {social}")


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(
    x = year,
    value = n,
    node = factor(gender),
    next_node = factor(gender)
  )
) +
  geom_segment(
    data = country_data,
    mapping = aes(
      x = year, xend = year,
      y = 0, yend = -375
    ),
    inherit.aes = FALSE,
    colour = alpha(text_col, 0.2),
    linewidth = 0.3
  ) +
  geom_textbox(
    data = country_data,
    mapping = aes(
      x = year,
      y = -380,
      label = country
    ),
    inherit.aes = FALSE,
    colour = alpha(text_col, 0.6),
    family = body_font,
    fill = "transparent",
    box.colour = "transparent",
    size = 5,
    hjust = 1,
    halign = 1,
    orientation = "left-rotated"
  ) +
  geom_sankey_bump(
    mapping = aes(fill = factor(gender)),
    space = 1,
    color = "transparent",
    smooth = 5,
    fill = "white",
    alpha = 1
  ) +
  geom_sankey_bump(
    mapping = aes(fill = factor(gender)),
    space = 1,
    color = "transparent",
    smooth = 5,
    alpha = 0.7
  ) +
  scale_fill_manual(
    values = col_palette
  ) +
  scale_y_continuous(
    limits = c(-825, 400)
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 24, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      size = rel(2),
      family = title_font,
      maxwidth = 0.8
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -70, t = 0),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.6
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 5, t = -65),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.9
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-09-24", paste0("20240924", ".png")),
  width = 8,
  height = 5
)

gg_playback(
  name = file.path("2024", "2024-09-24", paste0("20240924", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
