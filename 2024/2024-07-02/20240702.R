# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-07-02")
tt_datasets <- tuesdata$tt_datasets
tt_summary <- tuesdata$tt_summary
tt_urls <- tuesdata$tt_urls
tt_variables <- tuesdata$tt_variables


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
highlight_col <- "#388697"

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------

plot_data <- tt_datasets |>
  select(year, variables, observations)

labels_data <- plot_data |>
  group_by(year) |>
  slice_head(n = 1) |>
  ungroup()

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-07-02", "recording"),
  device = "png",
  width = 8,
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
title <- glue("<br><span style='font-size: 60px; font-weight: bold; font-family:{title_font};'>How big are TidyTuesday datasets?</span><br>")
st <- glue("<span style='font-family:{body_font};'>TidyTuesday is a weekly social data project organized by
the Data Science Learning Community. The over-arching goal of
TidyTuesday is to make learning to work with data easier, by
providing real-world datasets. New datasets (of varying sizes) are
posted each week! The maximum number of rows in any dataset is {format(max(plot_data$observations), big.mark = ',')} and the
maximum number of columns is {max(plot_data$variables)}.</span><br><br>")
cap <- paste0(
  title, st,
  "**Data**: {ttmeta}<br>**Graphic**:", social
)

# Plot --------------------------------------------------------------------

main_plot <- ggplot(data = plot_data) +
  geom_rect(
    mapping = aes(
      xmin = 0, xmax = variables,
      ymax = 0, ymin = -observations
    ),
    fill = highlight_col,
    colour = highlight_col,
    alpha = 0.3
  ) +
  labs(
    caption = cap
  ) +
  theme_void(base_size = 33, base_family = body_font) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(5, 5, 5, 5),
    strip.text = element_blank(),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 5, t = -110, r = 5),
      lineheight = 0.5,
      maxwidth = 0.63,
      family = body_font
    )
  )

main_plot +
  facet_wrap(~year) +
  geom_text(
    data = labels_data,
    mapping = aes(
      label = year,
      x = max(plot_data$variables) - 10,
      y = (-1 * max(plot_data$observations)) + 100000,
    ),
    family = title_font,
    size = 12,
    hjust = 1,
    vjust = 1
  )
ggsave("2024/2024-07-02/20240702.png", width = 8, height = 6)

main_plot 
ggsave("2024/2024-07-02/20240702_all.png", width = 8, height = 6)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-07-02", paste0("20240702", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
