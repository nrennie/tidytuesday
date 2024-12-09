# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(packcircles)
library(PrettyCols)
library(ggforce)
library(ggimage)
library(shadowtext)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-12-10")
parfumo_data_clean <- tuesdata$parfumo_data_clean


# Load fonts --------------------------------------------------------------

font_add_google("Libre Franklin", "libre")
font_add_google("Carter One")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- PrettyCols::prettycols("Summer")[12] |>
  usefunc::blend_palette("white") |>
  usefunc::blend_palette("white")
text_col <- "#99335F"
highlight_col <- PrettyCols::prettycols("Summer")[12]

body_font <- "libre"
title_font <- "Carter One"


# Data wrangling ----------------------------------------------------------

plot_data <- parfumo_data_clean |>
  select(Main_Accords) |>
  drop_na() |>
  separate_longer_delim(Main_Accords, delim = ", ") |>
  count(Main_Accords) |>
  slice_max(n, n = 10)

# pack circles
packing <- circleProgressiveLayout(
  plot_data$n,
  sizetype = "area"
)
circle_data <- cbind(plot_data, packing) |>
  as_tibble() |>
  arrange(desc(n)) |>
  mutate(
    rank = row_number()
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-12-10", "recording"),
  device = "png",
  width = 8,
  height = 5.2,
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
title <- "The Scent of Data"
st <- "Parfumo is a vibrant community of perfume enthusiasts, and data from their website shows the top 10 most popular main accords (primary scent characteristics) found in perfumes."
cap <- paste0(
  "**Data**: Parfumo | **Graphic**: ", social
)


# Plot --------------------------------------------------------------------

# Image from https://pixabay.com/illustrations/perfume-fragrant-scent-glamour-7773447/

# Plot
ggplot() +
  geom_circle(
    data = circle_data,
    mapping = aes(
      x0 = x,
      y0 = y,
      r = radius,
      fill = Main_Accords
    ),
    alpha = 0.6,
    colour = "transparent"
  ) +
  geom_shadowtext(
    data = circle_data,
    mapping = aes(
      x = x,
      y = y + 15,
      label = Main_Accords,
      size = case_when(
        rank <= 5 ~ 5.5,
        rank > 5 ~ 4
      )
    ),
    face = "bold",
    bg.color = bg_col,
    colour = text_col,
    hjust = 0.5,
    family = title_font
  ) +
  geom_shadowtext(
    data = circle_data,
    mapping = aes(
      x = x,
      y = y - 15,
      label = glue("{formatC(n, big.mark=',')}\nperfumes"),
      size = case_when(
        rank <= 5 ~ 4,
        rank > 5 ~ 3
      )
    ),
    bg.color = bg_col,
    colour = text_col,
    hjust = 0.5,
    lineheight = 0.8,
    family = title_font
  ) +
  geom_image(
    data = slice_head(plot_data, n = 1),
    aes(
      x = -300,
      y = -145,
      image = "2024/2024-12-10/perfume.png"
    ),
    size = 0.48
  ) +
  scale_x_continuous(limits = c(-400, 275)) +
  scale_y_continuous(limits = c(-230, 220)) +
  scale_size_identity() +
  scale_fill_pretty_d(
    palette = "Summer",
    direction = -1,
    guide = "none"
  ) +
  labs(title = title, subtitle = st, caption = cap) +
  coord_fixed() +
  theme_void(base_family = body_font, base_size = 12) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0),
      lineheight = 0.5,
      face = "bold",
      size = rel(1.8),
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -130, t = 0),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.45
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = -10, t = 5),
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2024", "2024-12-10", paste0("20241210", ".png")),
  width = 8,
  height = 5.2
)

gg_playback(
  name = file.path("2024", "2024-12-10", paste0("20241210", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
