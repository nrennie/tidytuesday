# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(gghalves)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-10-01")
chess <- tuesdata$chess


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
highlight_col <- "#AC3931"

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------


chess |> 
  select(victory_status, winner) |> 
  distinct()

plot_data <- chess |>
  select(game_id, opening_name, winner, white_rating, black_rating) |>
  #filter(white_rating >= 2000 & black_rating >= 2000) |>
  filter(str_detect(opening_name, "Sicilian Defense")) |> 
  pivot_longer(
    cols = c(white_rating, black_rating),
    names_to = "player",
    values_to = "rating"
  ) |>
  mutate(
    player = str_remove_all(player, "_rating"),
    player = factor(
      player,
      levels = c("white", "black"),
      labels = c("White\nplayer", "Black\nplayer")
    ),
    winner = factor(
      winner,
      levels = c("white", "draw", "black"),
      labels = c("White Player Wins", "Draw", "Black Player Wins")
    )
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-10-01", "recording"),
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
title <- "**Checkmate Champions** - does opponent rating matter?"
st <- "Data from just over 20,000 chess games, collected from a selection of users
on the site Lichess.org, shows the relationship between player ratings and who wins 
each game - for games that began with a Sicilian Defense (or variation). 
Higher rating numbers indicate stronger players. Though players with the 
higher rating tend to win, as you might expect, many lower rated players do go on 
to win. It also depends on who plays first. In chess, 
the player who moves first is **White** and the player who moves second is **Black**."
cap <- paste0(
  st, "<br>**Data**: Lichess.org<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_line(
    data = plot_data,
    mapping = aes(x = player, y = rating, group = game_id),
    alpha = 0.1
  ) +
  geom_half_violin(
    data = filter(plot_data, player == "Black\nplayer"),
    mapping = aes(x = player, y = rating),
    side = "r",
    fill = "black",
    alpha = 0.8
  ) +
  geom_half_violin(
    data = filter(plot_data, player == "White\nplayer"),
    mapping = aes(x = player, y = rating),
    side = "l",
    fill = "white",
    alpha = 0.8
  ) +
  facet_wrap(~winner, nrow = 1) +
  labs(
    title = title,
    subtitle = cap,
    x = "", y = "Player\nrating"
  ) +
  theme_minimal(
    base_family = body_font,
    base_size = 25
  ) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 0),
      lineheight = 0.5,
      size = rel(2.0),
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
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = alpha(text_col, 0.2),
      linewidth = 0.3
    ),
    axis.text.x = element_text(
      lineheight = 0.4,
      margin = margin(t = -5, b = -10)
    ),
    axis.title.y = element_text(
      lineheight = 0.4,
      angle = 0,
      face = "bold",
      vjust = 1.1,
      margin = margin(r = -18)
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-10-01", paste0("20241001", ".png")),
  width = 8,
  height = 5
)

gg_playback(
  name = file.path("2024", "2024-10-01", paste0("20241001", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
