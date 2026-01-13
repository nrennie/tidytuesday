# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(treemapify)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-01-20")
apod <- tuesdata$apod


# Functions ---------------------------------------------------------------

source("2026/2026-01-20/utils.R")


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#151C28"
text_col <- "#F2F4F8"
highlight_col <- "#B00783"


# Data wrangling ----------------------------------------------------------

input_data <- apod |>
  mutate(
    year = year(date)
  ) |>
  mutate(ftype = sub('.*\\.', '', url)) |> 
  filter(year == 2025, media_type == "image",ftype != "gif") |>
  arrange(date) |>
  mutate(id = row_number()) |>
  select(id, date, url, title) |>
  mutate(label = glue("**{title}**<br>{as.character(month(date, label = TRUE))} {lubridate::mday(date)}"))

# map over images
imgs <- input_data$url
all_imgs1 <- map_df(
  .x = 1:100,
  .f = ~ extract_data(.x)
)
all_imgs2 <- map_df(
  .x = 101:200,
  .f = ~ extract_data(.x)
)
all_imgs3 <- map_df(
  .x = 201:length(imgs),
  .f = ~ extract_data(.x)
)
all_imgs <- rbind(rbind(all_imgs1, all_imgs2), all_imgs3)

# save to file
write_csv(all_imgs, "2026/2026-01-20/data.csv")

# reread
all_imgs <- readr::read_csv("2026/2026-01-20/data.csv")

norm_imgs <- all_imgs |>
  group_by(id) |>
  mutate(tot_n = sum(n)) |>
  ungroup() |>
  mutate(prop = n / tot_n) |>
  left_join(
    input_data,
    by = "id"
  ) |>
  mutate(
    id = factor(id,
      levels = input_data$id,
      labels = input_data$label
    )
  ) |>
  mutate(
    month = month(date, label = TRUE),
    mday = mday(date)
  )

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2026", "2026-01-20", "recording"),
  device = "png",
  width = 12,
  height = 6,
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
title <- "The universe is brighter than you might think"
st <- "Each day NASA features a different image or photograph of our fascinating universe along with a brief explanation written by a professional astronomer. Each treemap shows different colours found in each day's featured image (excluding videos) throughout 2025."
cap <- paste0(
  "**Data**: NASA | **Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = norm_imgs,
  mapping = aes(
    area = prop, fill = hex,
    label = hex
  )
) +
  geom_treemap(colour = bg_col) +
  facet_grid(month ~ mday, switch = "y") +
  scale_fill_identity() +
  labs(title = title, subtitle = st, caption = cap) +
  theme_void(base_size = 12, base_family = body_font) +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(5, 15, 5, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    strip.text = element_text(
      size = rel(0.9), face = "bold",
      colour = text_col
    ),
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
      maxwidth = 0.9,
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
    )
  )

record_polaroid()


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2026", "2026-01-20", paste0("20260120", ".png")),
  width = 12,
  height = 6,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2026", "2026-01-20", paste0("20260120", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
