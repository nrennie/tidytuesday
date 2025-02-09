# Load packages -----------------------------------------------------------

library(tidyverse)
library(patchwork)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(waffle)


# Load data ---------------------------------------------------------------

# image from: https://ih1.redbubble.net/image.1926835005.0339/flat,750x,075,f-pad,750x1000,f8f8f8.jpg

tuesdata <- tidytuesdayR::tt_load("2025-02-04")
simpsons_characters <- tuesdata$simpsons_characters
simpsons_episodes <- tuesdata$simpsons_episodes
simpsons_locations <- tuesdata$simpsons_locations
simpsons_script_lines <- tuesdata$simpsons_script_lines


# Load fonts --------------------------------------------------------------

font_add("akbar", regular = "fonts/Akbar/akbar.ttf")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#fed41d"
text_col <- "#0f0400"
highlight_col <- "#ec59a0"

body_font <- "akbar"


# Data wrangling ----------------------------------------------------------

simpsons_data <- simpsons_script_lines |>
  filter(speaking_line) |>
  filter(raw_character_text %in% c("Homer Simpson", "Lisa Simpson", "Bart Simpson", "Marge Simpson")) |>
  select(episode_id, raw_character_text, word_count) |>
  drop_na() |>
  group_by(episode_id, raw_character_text) |>
  summarise(n = sum(word_count)) |>
  ungroup() |>
  group_by(raw_character_text) |>
  summarise(
    mean_n = mean(n),
    donut_n = round(mean_n / 10)
  ) |>
  arrange(desc(donut_n)) |>
  mutate(
    raw_character_text = factor(raw_character_text,
      levels = raw_character_text
    )
  )

basic_plot <- ggplot(
  data = simpsons_data,
  mapping = aes(fill = raw_character_text, values = donut_n)
) +
  geom_waffle(
    color = "white",
    size = .25,
    n_rows = 4,
    flip = FALSE
  ) +
  facet_wrap(
    ~raw_character_text,
    ncol = 1,
    strip.position = "left"
  )
basic_built <- ggplot_build(basic_plot)
new_data <- basic_built$data[[1]] |>
  dplyr::rename(raw_character_text = PANEL) |>
  dplyr::mutate(
    raw_character_text = factor(
      raw_character_text,
      labels = levels(simpsons_data$raw_character_text)
    )
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-02-04", "recording"),
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA,
  linkedin = NA
)
st <- "<span style='font-size:28pt;'>The Simpsons</span><br><br>The Simpsons is an animated, satirical depiction of American life, epitomised by the Simpson family, which consists of Homer, Marge, Bart, Lisa, and Maggie. On average Homer is the most talkative, speaking more than double the number of words of his wife, per episode. Maggie, as a one year old, has no spoken words recorded in the data, retrieved from Kaggle.com. This chart shows the average number of words per episode, where each donut represents 10 words."
cap <- paste0(
  st, "<br><br>**Graphic**:", social
)

# Plot --------------------------------------------------------------------

new_data <- new_data |>
  as_tibble() |>
  mutate(
    character_img = stringr::str_extract(raw_character_text, "^[^ ]+"),
    character_img = stringr::str_to_lower(character_img),
    character_img = glue("&nbsp;&nbsp;<img src='2025/2025-02-04/{character_img}.png' width='40'><br>{raw_character_text}"),
    .before = 6
  )

lvls <- new_data |>
  select(character_img, raw_character_text) |>
  distinct() |>
  arrange(raw_character_text) |>
  mutate(character_img = factor(character_img, levels = character_img)) |>
  pull(character_img)

new_data2 <- new_data |>
  mutate(
    character_img = factor(character_img, levels = lvls)
  ) |>
  select(x, y, character_img, raw_character_text)

g1 <- ggplot(
  data = new_data2
) +
  facet_wrap(
    ~character_img,
    ncol = 1,
    strip.position = "left"
  ) +
  theme_void(base_family = body_font, base_size = 10) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 15),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    strip.text.y = element_textbox_simple(
      vjust = 0,
      hjust = 0,
      margin = margin(l = 5, r = -20),
      width = 0.5
    )
  )
g1

g2 <- ggplot(
  data = new_data2,
  mapping = aes(
    x = x, y = y
  )
) +
  ggpattern::geom_tile_pattern(
    fill = "transparent",
    pattern = "image",
    pattern_filename = "2025/2025-02-04/donut.jpg"
  ) +
  facet_wrap(
    ~raw_character_text,
    ncol = 1,
    strip.position = "left"
  ) +
  labs(tag = cap) +
  coord_fixed() +
  theme_void(base_family = body_font, base_size = 9.5) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag.position = c(0.77, 0.37),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.67
    ),
    strip.text.y = element_textbox(
      hjust = 0.5,
      vjust = 0,
      width = 0.1,
      margin = margin(l = 5),
      colour = bg_col
    )
  )
g2

g2 + inset_element(g1,
  left = 0.02, right = 1, top = 1, bottom = 0,
  align_to = "plot",
  clip = FALSE
)
record_polaroid()


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-02-04", paste0("20250204", ".png")),
  height = 6,
  width = 6,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-02-04", paste0("20250204", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
