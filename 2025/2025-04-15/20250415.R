# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggdist)


# Load data ---------------------------------------------------------------

penguins


# Functions ---------------------------------------------------------------

weight_func <- function(x) {
  round(mean(filter(penguins, species %in% x)$body_mass, na.rm = T) / 1000, 1)
}


# Load fonts --------------------------------------------------------------

font_add_google(name = "Bungee", family = "bungee")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

highlight_col <- "darkorange"

# light mode
bg_col <- "#fafafa"
text_col <- "black"

# dark mode
bg_col <- "grey5"
text_col <- "white"

body_font <- "ubuntu"
title_font <- "bungee"


# Data wrangling ----------------------------------------------------------

plot_data <- penguins %>%
  select(species, body_mass) %>%
  drop_na()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-04-15", "recording"),
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
  mastodon = NA
)
title <- "Penguins"
st <- glue("Gentoo penguins have significantly higher body weights than Adelie or Chinstrap penguins, with an average mass of around {weight_func(c('Gentoo', 'Chinstrap'))}kg. In contrast, the average Adelie or Chinstrap penguin weighs around {weight_func('Adelie')}kg.")
cap <- paste0(
  "**Data**: R Datasets<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = species, y = body_mass / 1000, fill = species)
) +
  stat_gradientinterval(
    position = "dodge",
    colour = NA,
    width = 1
  ) +
  stat_halfeye(
    adjust = .3,
    width = .3,
    .width = 0,
    justification = -.3,
    point_colour = "NA",
    slab_fill = NA,
    slab_colour = text_col,
    slab_size = 0.4
  ) +
  geom_boxplot(
    width = .15,
    outlier.shape = NA,
    fill = bg_col,
    colour = text_col
  ) +
  stat_dots(
    side = "left",
    dotsize = .8,
    justification = 1.15,
    binwidth = 0.05,
    colour = text_col
  ) +
  coord_cartesian(
    xlim = c(1, NA),
    ylim = c(2, 7),
    clip = "off"
  ) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  guides(fill = "none", alpha = "none") +
  labs(
    x = "", y = "Body mass (g)",
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_minimal(base_family = body_font) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    text = element_text(colour = text_col),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      lineheight = 1,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 20),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.x = element_text(
      face = "bold",
      colour = text_col,
      size = rel(1.1)
    ),
    axis.text.y = element_text(
      colour = text_col
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha(text_col, 0.5)
    ),
    panel.grid.minor.y = element_line(
      colour = alpha(text_col, 0.25)
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-04-15", paste0("20250415", ".png")),
  height = 7,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-04-15", paste0("20250415", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
