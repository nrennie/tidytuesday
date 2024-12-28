# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(PrettyCols)
library(shadowtext)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-12-31")
book <- tuesdata$book
broadcast_media <- tuesdata$broadcast_media
journalism <- tuesdata$journalism
leadership <- tuesdata$leadership
restaurant_and_chef <- tuesdata$restaurant_and_chef


# Load fonts --------------------------------------------------------------

font_add_google(name = "Henny Penny", family = "henny")
font_add_google(name = "Cabin", family = "cabin")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#DBD3AD"
text_col <- "#7E5920"
highlight_col <- PrettyCols::prettycols("Celestial")[1]

body_font <- "cabin"
title_font <- "henny"


# Data wrangling ----------------------------------------------------------

ncats <- book |>
  filter(year == 2024) |> 
  pull(subcategory) |> 
  unique() |> 
  length()
nbooks <- nrow(filter(book, year == 2024))

plot_data <- book |>
  filter(year == 2024) |>
  select(subcategory, rank, name, title) |>
  group_by(subcategory) |> 
  mutate(cat_count = n()) |> 
  ungroup() |> 
  mutate(subcategory = case_when(
    cat_count < 5 ~ "Other",
    TRUE ~ subcategory
  )) |> 
  arrange(subcategory, desc(rank)) |>
  mutate(book_num = row_number()) |>
  group_by(subcategory) |>
  mutate(cat_num = row_number()) |>
  mutate(cat_mid = mean(book_num)) |>
  ungroup()

label_data <- plot_data |> 
  group_by(subcategory, rank) |> 
  mutate(
    subgroup_count = n(),
    subgroup_num = row_number()
  ) |> 
  ungroup() |> 
  select(subcategory, rank, book_num, subgroup_count, subgroup_num) |>
  group_by(subcategory, rank) |> 
  mutate(subgroup_mid = mean(book_num)) |> 
  ungroup() |> 
  select(subcategory, rank, subgroup_count, subgroup_mid) |> 
  distinct() |> 
  mutate(
    rank_label = case_when(
      subgroup_count > 1 ~ paste0(rank, "s"),
      TRUE ~ rank
    )
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-12-31", "recording"),
  device = "png",
  width = 8.5,
  height = 4,
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
title <- "James Beard Awards 2024: Books"
st <- glue("The James Beard Foundation Awards are annual awards presented by the James Beard Foundation to recognize chefs, restaurateurs, authors and journalists in the United States. For awards recognising book authors, each book is listed under a subcategory, such as *Baking and Desserts* or *Vegetable-Focused Cooking*. In 2024, there were {nbooks} books listed in {ncats} different subcategories. For this chart, subcategories with fewer than 5 books represented are aggregated.")
cap <- paste0(
  "**Data**: jamesbeard.org/awards<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  # bookshelf
  geom_rect(
    data = data.frame(),
    mapping = aes(
      xmin = -2,
      xmax = max(plot_data$book_num) + 3,
      ymin = -1.5,
      ymax = 0
    ),
    fill = text_col
  ) +
  # books
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = book_num - 0.5,
      xmax = book_num + 0.5,
      ymin = 0,
      ymax = 10 + as.numeric(I(rank == "Winner")),
      fill = subcategory,
      alpha = rank
    )
  ) +
  geom_textbox(
    data = filter(plot_data, cat_num == 1),
    mapping = aes(
      x = cat_mid,
      y = -0.75,
      label = subcategory
    ),
    alpha = 0.8,
    fill = bg_col,
    colour = text_col,
    size = 2.3,
    hjust = 0.5,
    halign = 0.5,
    vjust = 0.5,
    valign = 0.5,
    maxwidth = 0.085,
    minwidth = 0,
    minheight = 0.1,
    box.r = unit(3.5, "pt"),
    lineheight = 0.4,
    family = body_font,
    box.padding = unit(c(0.8, -0.3, 0.8, -0.3), "pt")
  ) +
  geom_shadowtext(
    data = label_data,
    mapping = aes(
      x = subgroup_mid,
      y = 8 + as.numeric(I(rank == "Winner")),
      label = subgroup_count
    ),
    family = title_font,
    size = 4,
    bg.color = bg_col,
    colour = text_col
  ) +
  geom_shadowtext(
    data = label_data,
    mapping = aes(
      x = subgroup_mid,
      y = 0.75,
      label = rank_label
    ),
    family = body_font,
    size = 3.3,
    hjust = 0,
    bg.color = bg_col,
    colour = text_col,
    angle = 90
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_alpha_discrete(
    range = c(0.4, 0.6)
  ) +
  scale_fill_pretty_d("Celestial") +
  coord_cartesian(expand = FALSE) +
  theme_void(base_family = body_font, base_size = 10) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 10),
      lineheight = 0.5,
      family = title_font,
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 0),
      lineheight = 0.55,
      maxwidth = 0.75,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2024", "2024-12-31", paste0("20241231", ".png")),
  width = 8.5,
  height = 4
)

gg_playback(
  name = file.path("2024", "2024-12-31", paste0("20241231", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
