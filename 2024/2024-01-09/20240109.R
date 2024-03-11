
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

# This fails :(
tuesdata <- tidytuesdayR::tt_load("2024-01-09")

# Load from CSV directly instead
canada_births_1991_2022 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/canada_births_1991_2022.csv')
nhl_player_births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv')
nhl_rosters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv')
nhl_teams <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_teams.csv')


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "ubuntu")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "black"
text_col <- "white"
highlight_col <- "grey50"

body_font <- "ubuntu"
title_font <- "ubuntu"


# Data wrangling ----------------------------------------------------------

nhl_data <- nhl_player_births |> 
  filter(birth_country == "CAN") |> 
  select(birth_date) |> 
  mutate(weekday = wday(birth_date),
         week = week(birth_date)) |> 
  group_by(weekday, week) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  complete(weekday, week) |> 
  mutate(n = replace_na(n, 0)) |> 
  mutate(n = ntile(n, 5))
 
greys <- data.frame(colour = grey.colors(5, start = 0.2),
                    n = 1:5)

plot_data <- nhl_data |> 
  left_join(greys, by = "n")

nhl_births <- nhl_player_births |> 
  select(birth_date) |> 
  mutate(month = month(birth_date),
         month_lab = month(birth_date, label = TRUE)) |> 
  group_by(month) |> 
  mutate(n_NHL = n()) |> 
  select(-birth_date) |> 
  unique() |> 
  ungroup() |> 
  mutate(n_NHL = ntile(n_NHL, 5))

births <- canada_births_1991_2022 |> 
  group_by(month) |> 
  summarise(n_Canada = sum(births)) |> 
  mutate(n_Canada = ntile(n_Canada, 5)) |> 
  left_join(nhl_births, by = "month") |> 
  pivot_longer(cols = c(n_Canada, n_NHL), names_to = "type", values_to = "n") |> 
  mutate(type = str_remove(type, "n_")) |> 
  left_join(greys, by = "n")


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-01-09", "recording"),
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
  font_family = body_font
)
title <- "More Canadian NHL players born at the start of the year"
st1 <- "Analysis of NHL players born in Canada between 1879 and 2005 reveals 
that a higher proportion of them are born in the first half of the year. Although 
this may be surprising, it's not clear if it's expected - are Canadian people often 
born in the first half of the year?"
st2 <- "When comparing to the general population of Cananda, where the summer 
months are the most common bith months, the pattern of NHL players being born 
in the first quarter of the year becomes clearer. These earlier birth months 
mean that, as children, they were older within their school year - and therefore 
more likely to make important teams at a young age, contributing to skill 
development."
cap <- paste0(
  "**Data**: NHL API<br>**Graphic**: ", social
)

# Legend data -------------------------------------------------------------

leg_data1 <- data.frame(
  week = 44:48,
  weekday = rep(9, 5),
  colour = greys$colour
)

leg_text1 <- data.frame(
  x = c(41.5, 49),
  y = c(9, 9),
  label = c("Less", "More")
)

leg_data2 <- data.frame(
  week = 7:11,
  weekday = rep(-1, 5),
  colour = greys$colour
)

leg_text2 <- data.frame(
  x = c(5.8, 11.7),
  y = c(-1, -1),
  label = c("Less", "More")
)

month_labels <- data.frame(
  x = week(ydm(paste0("2023-01-", 1:12))),
  y = rep(0, 12),
  label = month.abb
)


# Plot --------------------------------------------------------------------

ggplot() +
  statebins:::geom_rtile(
    data = plot_data,
    mapping = aes(x = week, y = weekday, fill = colour),
    radius = grid::unit(3, "pt"),
    width = 0.9,
    height = 0.9
  ) +
  statebins:::geom_rtile(
    data = leg_data1,
    mapping = aes(x = week, y = weekday, fill = colour),
    radius = grid::unit(3, "pt"),
    width = 0.9,
    height = 0.9
  ) +
  # Add legend labels
  geom_text(
    data = leg_text1,
    mapping = aes(x = x, y = y, label = label),
    family = body_font,
    colour = text_col,
    size = 12,
    hjust = 0
  ) +
  # Add month labels
  geom_text(
    data = month_labels,
    mapping = aes(x = x, y = y, label = label),
    family = body_font,
    colour = text_col,
    size = 12,
    hjust = -0.5
  ) +
  scale_fill_identity() +
  scale_y_reverse(breaks = c(2, 4, 6),
                  labels = c("Mon", "Wed", "Fri")) +
  coord_fixed() +
  labs(title = title,
       subtitle = st1,
       caption = cap) +
  theme_void(base_size = 32, base_family = body_font) +
  theme(
    plot.margin = margin(10, -15, 10, 15),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.text.y = element_text(colour = text_col,
                               hjust = 0,
                               margin = margin(r = -20)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      size = rel(1.8),
      face = "bold",
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 65, t = 10, r = 25),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -10, t = 60),
      lineheight = 0.5,
      family = body_font
    )
  )

# month comparison plot
ggplot() +
  statebins:::geom_rtile(
    data = births,
    mapping = aes(x = month_lab, y = type, fill = colour),
    radius = grid::unit(3, "pt"),
    width = 0.9,
    height = 0.9
  ) +
  statebins:::geom_rtile(
    data = leg_data2,
    mapping = aes(x = week, y = weekday, fill = colour),
    radius = grid::unit(3, "pt"),
    width = 0.9,
    height = 0.9
  ) +
  # Add legend labels
  geom_text(
    data = leg_text2,
    mapping = aes(x = x, y = y, label = label),
    family = body_font,
    colour = text_col,
    size = 12,
    hjust = 0
  ) +
  scale_fill_identity() +
  scale_x_discrete(position = "top") +
  coord_fixed() +
  labs(title = title,
       subtitle = st2,
       caption = cap) +
  theme_void(base_size = 32, base_family = body_font) +
  theme(
    plot.margin = margin(10, 10, 10, 15),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.text.x = element_text(colour = text_col,
                               hjust = 0.5),
    axis.text.y = element_text(colour = text_col,
                               hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      size = rel(1.8),
      face = "bold",
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 20, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )



# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-01-09", paste0("20240109", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
