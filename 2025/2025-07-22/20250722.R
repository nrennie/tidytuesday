# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggiraph)
library(tidytext)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-07-22")
mta_art <- tuesdata$mta_art
station_lines <- tuesdata$station_lines


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "grey5"
text_col <- "white"
highlight_col <- "#35978f"

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------

# clean art data
art_data <- mta_art |>
  filter(agency == "NYCT") |>
  arrange(art_date) |>
  group_by(art_date) |>
  mutate(y = row_number()) |>
  ungroup() |>
  select(art_date, y, art_title, artist, art_material, art_description) |>
  mutate(id = row_number())

# data for sentiment analysis
data(stop_words)
afinn_df <- get_sentiments("afinn")
text_df <- art_data |>
  select(id, art_description) |>
  unnest_tokens(word, art_description) |>
  anti_join(stop_words, by = "word")
sentiment_df <- text_df |>
  inner_join(afinn_df, by = "word") |>
  group_by(id) %>%
  summarise(mean_sent = mean(value, na.rm = T)) %>%
  drop_na()

# join
plot_data <- art_data |>
  left_join(sentiment_df, by = "id") |>
  mutate(
    sent_label = if_else(is.na(mean_sent), "<i>Unknown</i>", as.character(round(mean_sent, 1))),
    tooltip_label = glue("<b>{art_title}</b><br><i>{artist}. {art_date}.</i><br>Sentiment: {sent_label}")
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-07-22", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
title <- "Art in New York City Transit stations is mostly positive"
st <- glue("Sentiment analysis of description of art works installed in New York City Transit stations reveals that they are more likely to be <span style='color:{highlight_col};'>**positive**</span> than <span style='color:#bf812d;'>**negative**</span>.")
cap <- paste0(
  "**Data**: MTA Permanent Art Catalog<br>**Graphic**:", social
)
tag <- glue("<span style='font-family:{title_font};font-size:24px'>**{title}**</span><br><br>{st}<br><br>{cap}")


# Plot --------------------------------------------------------------------

g <- ggplot() +
  # geom_point(
  #   mapping = aes(fill = mean_sent, colour = mean_sent),
  #   size = 3.8,
  #   pch = 21
  # ) +
  geom_point_interactive(
    data = plot_data,
    mapping = aes(
      x = art_date, y = y, fill = mean_sent, colour = mean_sent, tooltip = tooltip_label,
      data_id = id
    ),
    size = 3.8,
    pch = 21
  ) +
  labs(
    tag = tag,
    x = NULL, y = NULL
  ) +
  scale_fill_distiller(
    type = "div", palette = "BrBG", direction = 1,
    limits = c(-3, 3),
    na.value = bg_col
  ) +
  scale_colour_distiller(
    type = "div", palette = "BrBG", direction = 1,
    limits = c(-3, 3),
    na.value = text_col
  ) +
  theme_minimal(
    base_size = 11, base_family = body_font
  ) +
  theme(
    text = element_text(colour = text_col),
    legend.position = "none",
    plot.margin = margin(5, 5, 10, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag.position = c(0.04, 0.75),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 1,
      family = body_font,
      maxwidth = 0.7,
      size = rel(1)
    ),
    panel.grid = element_blank(),
    axis.text.x = element_text(colour = text_col),
    axis.text.y = element_blank()
  )
g_interactive <- girafe(
  ggobj = g, width_svg = 7,
  height_svg = 5,
  bg = bg_col,
  options = list(
    opts_tooltip(
      use_fill = FALSE,
      use_stroke = FALSE,
      css = glue("
        padding: 5pt;
        font-family: {body_font};
        font-size: 1rem;
        color: {text_col};
        background-color: rgba(0, 0, 0, 1.0) !important;
        border: solid;
        border-color: {highlight_col};
        border-width: 2px;")
    ),
    opts_hover(
      css = girafe_css(
        css = glue("stroke-width:2px"),
      )),
    opts_toolbar(saveaspng = FALSE),
    opts_zoom(max = 1),
    opts_sizing(rescale = FALSE, width = 0.8)
  )
)
g_interactive


# Save gif ----------------------------------------------------------------

ggsave(
  plot = g,
  filename = file.path("2025", "2025-07-22", paste0("20250722", ".png")),
  height = 5,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-07-22", paste0("20250722", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
