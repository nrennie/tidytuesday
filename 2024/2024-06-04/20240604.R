
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggimage)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-06-04")
cheeses <- tuesdata$cheeses


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()

body_font <- "roboto"
title_font <- "roboto_slab"


# Define colours ----------------------------------------------------------

bg_col <- "white"
text_col <- "grey5"
col_palette <- c("#F3B61F", "#008148", "#034732", "#96ACB7")
names(col_palette) <- c(
  "Neither vegetarian or vegan",
  "Vegetarian but not vegan",
  "Vegan",
  "Unknown"
)

highlight_col <- col_palette[1]
  
# Data wrangling ----------------------------------------------------------

# From: https://github.com/hrbrmstr/waffle/blob/master/R/utils.r
round_preserve_sum <- function(x, digits = 0) {
  up <- 10^digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

cheese_data <- cheeses |> 
  mutate(
    type = case_when(
      !vegan & !vegetarian ~ "Neither vegetarian or vegan",
      !vegan & vegetarian ~ "Vegetarian but not vegan",
      vegan & vegetarian ~ "Vegan",
      is.na(vegan) & is.na(vegetarian) ~ "Unknown"
    )
  ) |> 
  count(type) |> 
  mutate(p = n / sum(n),
         p = 100 * round_preserve_sum(p, digits = 2)) |> 
  mutate(
    type = factor(type, levels = c(
      "Neither vegetarian or vegan",
      "Vegetarian but not vegan",
      "Vegan",
      "Unknown"
    ))
  ) |> 
  arrange(type)
  
plot_data <- cheese_data |> 
  select(-n) |> 
  uncount(p) |> 
  mutate(
    x = rep(1:10, times = 10),
    y = rep(1:10, each = 10)
  ) 


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-06-04", "recording"),
  device = "png",
  width = 5,
  height = 7,
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
title <- "Is your cheese really vegetarian?"
st <- glue::glue("Cheese is traditionally made mostly from the milk of cows but also other 
mammals, including sheep, goats, buffalo, reindeer, camels and yaks. Around 4,000 
years ago people started to breed animals and process their milk. That's 
when the cheese was born. Of {format(nrow(cheeses), big.mark = ',')} cheeses listed on cheese.com, there was a 
mixture of 
<span style='color:{col_palette[[1]]}'>**{str_to_lower(names(col_palette)[1])}**</span>, 
<span style='color:{col_palette[[2]]}'>**{str_to_lower(names(col_palette)[2])}**</span>, and
<span style='color:{col_palette[[3]]}'>**{str_to_lower(names(col_palette)[3])}**</span> cheeses, 
and many with an <span style='color:{col_palette[[4]]}'>**{str_to_lower(names(col_palette)[4])}**</span> status. 
Vegan cheeses represented around 0.5% of listed cheeses, and so are not represented in the chart below.")
# what perc of known not vege
# unknown rephrase

cap <- paste0(
  "**Data**: cheese.com | **Image**: Unsplash <br>**Graphic**:", social
)



# Image -------------------------------------------------------------------

# From https://unsplash.com/photos/sliced-cheese-on-black-plate-beside-wine-glass-jeAjT87nbjM


# Plot --------------------------------------------------------------------

ggplot() +
  # add cheese photo
  geom_image(
    data = slice_head(plot_data, n = 1),
    aes(
      x = 5.5,
      y = 5.5,
      image = "2024/2024-06-04/cheese.jpg"
    ),
    size = 1.1
  ) +
  # heatmap
  geom_tile(
    data = plot_data,
    mapping = aes(
      x = x, y = y, fill = type
    ),
    colour = bg_col,
    linewidth = 2,
    alpha = 0.5
  ) +
  # styling
  scale_fill_manual(values = col_palette, drop = FALSE) +
  coord_fixed(expand = FALSE) +
  labs(title = title,
       subtitle = st,
       caption  = cap) +
  theme_void(base_family = body_font, base_size = 24) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 15),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 15),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-06-04", paste0("20240604", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
