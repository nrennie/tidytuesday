# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggalluvial)
library(ggimage)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-12-17")
spells <- tuesdata$spells


# Load fonts --------------------------------------------------------------

font_add_google("Jolly Lodger", "jolly")
font_add_google(name = "Cabin", family = "cabin")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "gray5"
text_col <- "gray95"
highlight_col <- "#ABFF4F"

body_font <- "cabin"
title_font <- "jolly"


# Data wrangling ----------------------------------------------------------

plot_data <- spells |>
  select(school, bard:wizard) |>
  pivot_longer(-school) |>
  filter(value) |>
  select(-value) |>
  count(school, name) |>
  mutate(
    across(c(school, name), str_to_title)
  )

name_levels <- plot_data |>
  group_by(name) |>
  summarise(n = sum(n)) |>
  arrange(n) |>
  pull(name)

plot_data$name <- factor(plot_data$name, levels = name_levels)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-12-17", "recording"),
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
  font_family = body_font,
  mastodon = NA
)
title <- "Who Can Cast What? A Guide to D&D Spell Types"
st <- glue("Many characters have the ability to cast spells, which have a huge variety of effects. Some spells are mostly useful in combat, by dealing damage or imposing conditions. Other spells have utility in exploration. If you’re playing a spellcaster, look for a mix of combat-effective and utilitarian spells to help deal with varied challenges. As you might expect, <span style='color:{highlight_col}'>**wizards**</span> can cast the widest range of spells.")
cap <- paste0(
  "**Data**: dndbeyond.com | **Image**: pixabay.com | **Graphic**:", social
)


# Plot --------------------------------------------------------------------

# Image: https://pixabay.com/illustrations/wizard-fantasy-magic-mystery-spell-4417430/

ggplot(
  data = plot_data,
  mapping = aes(
    axis1 = name, axis2 = school,
    y = n
  )
) +
  geom_alluvium(
    mapping = aes(fill = name),
    alpha = 0.9,
    width = 0.25
  ) +
  geom_stratum(
    fill = "gray15",
    colour = alpha(text_col, 0.5),
    width = 0.25
  ) +
  geom_text(
    stat = "stratum",
    mapping = aes(label = str_wrap(after_stat(stratum), 10)),
    family = body_font,
    size = 3.5,
    colour = text_col,
    lineheight = 0.5
  ) +
  geom_image(
    data = slice_head(plot_data, n = 1),
    aes(
      x = 0.65,
      y = 350,
      image = "2024/2024-12-17/wizard.png"
    ),
    size = 0.5
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_x_discrete(
    limits = c("name", "school"),
    labels = str_wrap(c(
      "Who is casting...",
      "...each type of spell?"
    ), 18),
    position = "top",
    expand = expansion(mult = 0, add = c(0.5, 0))
  ) +
  scale_fill_manual(
    values = rev(c(
      highlight_col,
      "grey30", "grey40", "grey50", "grey60",
      "grey70", "grey80", "grey90"
    ))
  ) +
  theme_void(base_family = body_font, base_size = 10) +
  theme(
    plot.margin = margin(5, 10, 5, 10),
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.text.x = element_text(
      family = title_font,
      colour = text_col,
      lineheight = 0.7,
      size = rel(1.2),
      margin = margin(b = 2)
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      family = title_font,
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2024", "2024-12-17", paste0("20241217", ".png")),
  width = 7,
  height = 5
)

gg_playback(
  name = file.path("2024", "2024-12-17", paste0("20241217", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
