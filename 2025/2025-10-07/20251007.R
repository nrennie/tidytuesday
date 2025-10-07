# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggnewscale)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-10-07")
euroleague_basketball <- tuesdata$euroleague_basketball


# Load fonts --------------------------------------------------------------

font_add(
  family = "fa_regular",
  regular = "fonts/Font Awesome 7 Free-Solid-900.otf"
)
font_add_google("Contrail One", "contrail")
font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "grey10"
text_col <- "grey95"
highlight_col <- "#D7331D"

body_font <- "lato"
title_font <- "contrail"


# Data wrangling ----------------------------------------------------------

stadium_data <- euroleague_basketball |>
  select(Team, Capacity) |>
  separate_longer_delim(Capacity, ", ") |>
  mutate(Capacity = parse_number(Capacity))

final_data <- euroleague_basketball |>
  select(Team, Years_of_FinalFour_Appearances) |>
  separate_longer_delim(Years_of_FinalFour_Appearances, ", ") |>
  mutate(type1 = if_else(
    !is.na(Years_of_FinalFour_Appearances), "Final", "No-Final"
  )) |>
  rename(Year = Years_of_FinalFour_Appearances)

win_data <- euroleague_basketball |>
  select(Team, Years_of_Titles_Won) |>
  mutate(Years_of_Titles_Won = if_else(
    Years_of_Titles_Won == "None", NA, Years_of_Titles_Won
  )) |>
  separate_longer_delim(Years_of_Titles_Won, ", ") |>
  mutate(type2 = if_else(
    !is.na(Years_of_Titles_Won), "Win", "No-Win"
  )) |>
  rename(Year = Years_of_Titles_Won)

wins_levels <- euroleague_basketball |> 
  select(Team, Titles_Won, FinalFour_Appearances) |> 
  arrange(-Titles_Won, -FinalFour_Appearances) |> 
  pull(Team)

plot_data <- final_data |>
  full_join(
    win_data,
    by = c("Team", "Year")
  ) |>
  mutate(
    type = case_when(
      type2 == "Win" ~ "Win",
      type1 == "Final" ~ "Final"
    )
  ) |>
  select(Team, Year, type) |>
  drop_na() |>
  mutate(Team = factor(Team, levels = wins_levels)) |> 
  complete(Team, Year, fill = list(type = "None")) |>
  mutate(Year = as.numeric(Year))

icon_data <- plot_data |> 
  filter(type != "None")


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-10-07", "recording"),
  device = "png",
  width = 7,
  height = 7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Better team, bigger stadium? Not so much."
st <- glue("The EuroLeague is the top-tier European professional basketball club competition, widely regarded as the most prestigious competition in European basketball. Only a subset of teams have ever made it into the <span style='color:#D69F1F'>**final four**</span>, and fewer still have gone onto <span style='color:{highlight_col}'>**win**</span>. A team's home stadium capacity and their win rate don't seem to impact each other.")
cap <- paste0(
  "**Data**: EuroleagueBasketball R package | **Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_line(
    data = plot_data,
    mapping = aes(x = Year, y = Team),
    colour = alpha(text_col, 0.3),
    linewidth = 0.5
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(x = Year, y = Team, colour = (type == "None")),
    fill = bg_col,
    pch = 21,
    size = 2.7
  ) +
  scale_colour_manual(
    values = c(bg_col, alpha(text_col, 0.3))
  ) +
  new_scale_colour() +
  geom_textbox(
    data = icon_data,
    mapping = aes(
      x = Year, y = Team, colour = type,
      label = glue(
        "<span style='font-family:\"fa_regular\";'>&#xf434;</span>"
      )
    ),
    fill = "transparent",
    family = body_font,
    box.colour = "transparent",
    width = 0.18,
    hjust = 0.5,
    size = 2.8,
    halign = 0.5,
    vjust = 0.5,
    valign = 0.5
  ) +
  scale_colour_manual(
    values = c("#D69F1F", highlight_col)
  ) +
  geom_point(
    data = stadium_data,
    mapping = aes(x = 2032, y = Team, size = Capacity),
    colour = text_col,
    fill = text_col,
    alpha = 0.3,
    pch = 21
  ) +
  scale_x_continuous(
    breaks = c(seq(1990, 2025, 5), 2032),
    labels = c(seq(1990, 2025, 5), "Stadium\ncapacity"),
    expand = expansion(0, c(0.5, 1.5))
  ) +
  scale_y_discrete(
    limits = rev
  ) +
  scale_size(range = c(1, 7)) +
  labs(
    x = NULL, y = NULL, title = title,
    subtitle = st, caption = cap
  ) +
  theme_gray(base_family = body_font, base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 15, 10, 10),
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
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(colour = text_col)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-10-07", paste0("20251007", ".png")),
  width = 7,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-10-07", paste0("20251007", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
