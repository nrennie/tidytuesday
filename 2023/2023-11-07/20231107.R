# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(geofacet)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-07")
house <- tuesdata$house


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Carter One", "carter")
showtext_auto()


# Define colours and fonts-------------------------------------------------

text_col <- "#202A44"
blue_col <- "#0015BC"
red_col <- "#C41E3A"
bg_col <- "#fafafa"

body_font <- "roboto"
title_font <- "carter"


# Data wrangling ----------------------------------------------------------

plot_data <- house |>
  mutate(party = case_when(
    party == "REPUBLICAN" ~ "Republican",
    party == "DEMOCRAT" ~ "Democrat",
    TRUE ~ "Other"
  )) |>
  filter(stage == "GEN") |> 
  group_by(year, state_po, party) |>
  summarise(votes = sum(candidatevotes)) |> 
  ungroup()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-11-07", "recording"),
  device = "png",
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = blue_col,
  font_colour = text_col,
  font_family = body_font
)
st <- glue("Areas indicate the percentage of votes for <span style='color:{blue_col};'>Democrat</span>, <span style='color:{red_col};'>Republican</span>, and 
           <span style='color:#aaaaaa;'>Other</span> parties in general elections between 
           1976 and 2022.")
cap <- paste0(
  st,
  "<br><br>",
  "**Data**: U.S. House 1976–2022. MIT Election Data and Science Lab."
)
title <- "US House Election Results"


# Plot --------------------------------------------------------------------

# inset plot
p_inset <- ggplot(data = filter(plot_data, state_po == "CA")) +
  geom_area(
    mapping = aes(
      x = year,
      y = votes,
      fill = party
    ),
    position = "fill"
  ) +
  # label for state
  geom_text(
    mapping = aes(
      x = mean(range(year)),
      y =  0.5,
      label = state_po
    ),
    family = title_font,
    colour = alpha(bg_col, 0.7),
    size = 50
  ) +
  # year labels
  geom_text(
    mapping = aes(
      x = min(year) + 5,
      y =  0.1,
      label = min(year)
    ),
    family = body_font,
    colour = alpha(bg_col, 0.7),
    size = 14
  ) +
  geom_text(
    mapping = aes(
      x = max(year) - 5,
      y =  0.1,
      label = max(year)
    ),
    family = body_font,
    colour = alpha(bg_col, 0.7),
    size = 14
  ) +
  scale_fill_manual(
    values = c("Democrat" = blue_col, "Republican" = red_col, "Other" = "#aaaaaa")
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col)
  )


# main plot
p_main <- ggplot(data = plot_data) +
  geom_area(
    mapping = aes(
      x = year,
      y = votes,
      fill = party
    ),
    position = "fill"
  ) +
  geom_text(
    mapping = aes(
      x = mean(range(year)),
      y =  0.5,
      label = state_po
    ),
    family = title_font,
    colour = alpha(bg_col, 0.7),
    size = 15
  ) +
  scale_fill_manual(
    values = c("Democrat" = blue_col, "Republican" = red_col, "Other" = "gray50")
  ) +
  facet_geo(~state_po, grid = "us_state_grid2") +
  coord_cartesian(expand = FALSE) +
  labs(
    title = title,
    tag = cap,
    caption = social
  ) +
  theme_void(base_size = 30) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(10, 10, 10, 300),
    legend.position = "none",
    plot.title = element_textbox_simple(
      hjust = -0.9,
      halign = -0.9,
      colour = text_col,
      face = "bold",
      family = title_font,
      lineheight = 0.5,
      size = 60,
      margin = margin(b = 20)
    ),
    plot.tag = element_textbox_simple(
      hjust = 0,
      colour = text_col,
      size = 35,
      maxwidth = 0.65,
      lineheight = 0.5,
      family = body_font,
      margin = margin(b = 20)
    ),
    plot.caption = element_textbox_simple(
      hjust = 1,
      halign = 1,
      colour = text_col,
      size = 34,
      maxwidth = 0.65,
      lineheight = 0.5,
      family = body_font,
      margin = margin(b = 5, t = 10)
    ),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    plot.tag.position = c(-0.51, 0.76)
  )
record_polaroid()

# combine plots
p_main + inset_element(p_inset, 0.02, 0.087, 0.32, 0.62, align_to = "full", clip = FALSE)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-11-07", paste0("20231107", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
