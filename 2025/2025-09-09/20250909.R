# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggbump)
library(rcartocolor)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-09-09")
country_lists <- tuesdata$country_lists
rank_by_year <- tuesdata$rank_by_year


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "black"
highlight_col <- carto_pal(3, "Bold")[1]

body_font <- "Ubuntu"
title_font <- "Ubuntu"


# Data wrangling ----------------------------------------------------------

plot_data_1 <- rank_by_year |>
  mutate(region = str_to_title(region),
         colour_region = region)
plot_data_2 <- plot_data_1 |> 
  mutate(region = "All")
plot_data <- rbind(plot_data_1, plot_data_2)
top_countries <- plot_data_1 |>
  filter(year == 2025) |>
  group_by(region) |>
  slice_min(rank, with_ties = FALSE) |>
  ungroup() |>
  arrange(rank)
region_order <- c("All", top_countries$region)
highlight_data <- plot_data |>
  filter(country %in% top_countries$country) |>
  mutate(region = factor(region, levels = region_order))
plot_data <- plot_data |>
  mutate(region = factor(region, levels = region_order))

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-09-09", "recording"),
  device = "png",
  width = 8,
  height = 8,
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
title <- "Holders of Singaporean passports can travel to the most countries visa free, but European passports remain strong"
st <- "The Henley Passport Index is produced by Henley & Partners and captures the number of countries to which travelers in possession of each passport in the world may enter visa free."
cap <- paste0(
  "**Data**: Henley Passport Index API  | **Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  # Background lines
  geom_bump(
    data = select(plot_data, -region),
    mapping = aes(x = year, y = rank, group = country),
    colour = alpha("grey", 0.2),
    linewidth = 0.3
  ) +
  # coloured layers
  geom_bump(
    data = plot_data,
    mapping = aes(
      x = year, y = rank,
      group = country, colour = colour_region
    ),
    alpha = 0.4,
    linewidth = 0.4
  ) +
  # coloured highlight layers
  geom_bump(
    data = filter(highlight_data, region != "All"),
    mapping = aes(
      x = year, y = rank,
      group = country, colour = colour_region
    ),
    linewidth = 1
  ) +
  # labels
  geom_text(
    data = filter(highlight_data, year == 2025, region != "All"),
    mapping = aes(
      x = year + 0.1,
      label = str_wrap(country, 5),
      y = rank,
      colour = colour_region
    ),
    family = body_font,
    hjust = 0,
    vjust = 0,
    size = 2.5,
    fontface = "bold",
    lineheight = 0.8
  ) +
  geom_text(
    data = filter(highlight_data, year == 2025, region != "All"),
    mapping = aes(
      x = year + 0.1,
      label = str_wrap(
        paste0(visa_free_count, " countries"), 5
      ),
      y = rank,
      colour = colour_region
    ),
    family = body_font,
    hjust = 0,
    vjust = 1.2,
    size = 2.3,
    lineheight = 0.8
  ) +
  facet_wrap(~region, nrow = 2) +
  scale_colour_manual(
    values = carto_pal(8, "Bold")[1:7]
  ) +
  scale_x_continuous(
    limits = c(2005, 2032),
    breaks = c(2005, 2015, 2025),
    labels = c("2005", "'15", "'25")
  ) +
  scale_y_reverse(
    limits = c(120, 0),
    breaks = c(110, 0),
    labels = c("Least\npowerful", "Most\npowerful")
  ) +
  labs(
    x = NULL, y = NULL, title = title,
    subtitle = st, caption = cap
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_family = body_font, base_size = 10) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.2)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text.x = element_text(hjust = 0, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.spacing = unit(0.3, "lines")
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-09-09", paste0("20250909", ".png")),
  width = 8,
  height = 8,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-09-09", paste0("20250909", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
