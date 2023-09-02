# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(emojifont)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-08-29")
fair_use_cases <- tuesdata$fair_use_cases
fair_use_findings <- tuesdata$fair_use_findings

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "grey10"
text_col <- "grey90"
green <- "#84C318"
red <- "#9B1D20"

# Data wrangling ----------------------------------------------------------

# Internet/Digitization/Computer program
cases_data <- fair_use_cases |>
  filter(year >= 1980,
         outcome == "Fair use found" | outcome == "Fair use not found") |>
  select(year, categories, fair_use_found) |>
  mutate(
    tech = case_when(
      str_detect(categories, "Computer program|Internet") ~ "Digital",
      TRUE ~ "Non-digital"
    )
  ) |>
  filter(tech == "Digital") |> 
  select(year, fair_use_found) |>
  group_by(year, fair_use_found) |>
  summarise(n = n()) |> 
  ungroup() 

plot_data <- cases_data |> 
  uncount(n) |> 
  group_by(year, fair_use_found) |> 
  mutate(y = row_number()) |> 
  ungroup() |> 
  mutate(y = case_when(
    !fair_use_found ~ y * -1,
    TRUE ~ y
  )) |> 
  mutate(icon = case_when(
    !fair_use_found ~ fontawesome("fa-times"),
    fair_use_found ~ fontawesome("fa-check")
  ))
  


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-08-29", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = green,
  font_colour = text_col,
  font_family = "roboto"
)
title <- "Fair Use"
st <- "Fair use is a longstanding and vital aspect of American copyright law. 
The goal of the U.S. Copyright Office Fair Use Index is to make the principles 
and application of fair use more accessible and understandable to the public by 
presenting a searchable database of court opinions. There are an increasing 
number of cases which fall into the category of computer program and/or internet 
and digitization."
cap <- paste0(
  "**Data**: U.S. Copyright Office Fair Use Index<br>", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  # icons
  geom_text(
    data = plot_data,
    mapping = aes(x = year, y = y, label = icon, colour = fair_use_found),
    family = "fontawesome-webfont",
    size = 11) +
  # year labels
  geom_text(
    data = data.frame(x = seq(1990, 2020, by = 5)),
    mapping = aes(x = x, y = 0, label = x),
    colour = text_col,
    size = 7) +
  geom_point(
    data = data.frame(x = setdiff(1990:2022, seq(1990, 2020, by = 5))),
    mapping = aes(x = x, y = 0),
    colour = alpha(text_col, 0.6),
    size = 0.3) +
  # legend
  geom_text(
    data = data.frame(x = c(1993.5, 2000.25), y = 2.5,
                      fair_use_found = c(TRUE, FALSE),
                      label = c(fontawesome("fa-check"), fontawesome("fa-times"))),
    mapping = aes(x = x, y = y, label = label, colour = fair_use_found),
    family = "fontawesome-webfont",
    size = 11) +
  geom_text(
    data = data.frame(x = c(1990, 1996), y = 2.5,
                      label = c("Fair use found", "Fair use not found")),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = "roboto",
    hjust = 0.35, 
    size = 10) +
  # styling
  labs(title = title, 
       subtitle = st,
       caption = cap) +
  scale_colour_manual(values = c("TRUE" = green, "FALSE" = red)) +
  theme_void(base_size = 30, base_family = "roboto") +
  theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col),
        plot.margin = margin(10, 10, 10, 15),
        legend.position = "none",
        plot.title = element_textbox_simple(
          hjust = 0,
          halign = 0,
          colour = text_col,
          size = 54,
          face = "bold",
          lineheight = 0.5,
          family = "robotoslab",
          margin = margin(b = 5, t = 5)
        ),
        plot.subtitle = element_textbox_simple(
          hjust = 0,
          halign = 0,
          width = 0.8,
          colour = text_col,
          lineheight = 0.5,
          family = "roboto",
          margin = margin(b = -40, t = 15)
        ),
        plot.caption = element_textbox_simple(
          hjust = 0,
          halign = 0,
          colour = text_col,
          lineheight = 0.5,
          family = "roboto",
          margin = margin(b = 0, t = -15)
        )
        )

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-08-29", paste0("20230829", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
