# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggpattern)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-08-19")
scottish_munros <- tuesdata$scottish_munros


# Load fonts --------------------------------------------------------------

font_add_google("Jost", "jost")
font_add(
  family = "fa_regular",
  regular = "fonts/Font Awesome 7 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "grey90"
text_col <- "#0C3745"
highlight_col <- "#45465E"

body_font <- "jost"
title_font <- "jost"


# Data wrangling ----------------------------------------------------------

plot_data <- scottish_munros |>
  select(Name, Height_ft, `2021`) |>
  mutate(
    `2021` = replace_na(`2021`, "Unclassified"),
  ) |>
  rename(
    Classification = `2021`
  ) |>
  filter(Classification == "Munro")

dens <- density(plot_data$Height_ft)
y_at_mean <- approx(dens$x, dens$y, xout = mean(plot_data$Height_ft))$y
y_at_med <- approx(dens$x, dens$y, xout = median(plot_data$Height_ft))$y


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-08-19", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- glue("<span style='font-family:\"Font Awesome 6 Brands\";color:{highlight_col};'>&#xf09b;</span><span style='color:{bg_col};'>.</span><span style='font-family:jost;color:{text_col};'>nrennie</span>")
title <- "<span style='font-size: 20pt;'>**How tall are Scotland's Munros?**</span>"
st <- "<br><br>In 1891, Sir Hugh Munro produced the first list of Munros which are Scottish mountains with elevation of over 3,000 feet. Unlike other classification schemes which require a peak to have a prominence of at least 500 feet for inclusion, the Munros lack a rigid set of criteria. A *Munro Top* is a subsidiary summit of a Munro that also exceeds 3,000 feet in height but is not considered a distinct mountain in its own right. "
cap <- paste0(
  title, st, "<br><br>**Data**: The Database of British and Irish Hills v18.2<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = Height_ft)
) +
  geom_density(fill = "#616283", colour = "#45465E") +
  geom_density_pattern(
    pattern = "gradient",
    pattern_fill = NA,
    pattern_fill2 = "white",
    pattern_orientation = "vertical",
    fill = NA,
    colour = NA
  ) +
  geom_textbox(
    data = slice_max(plot_data, Height_ft),
    mapping = aes(
      x = Height_ft, y = 0.0001,
      label = glue(
        "<span style='font-family:\"fa_regular\";color:{highlight_col};'>&#xf6fc;</span>"
      )
    ),
    fill = "transparent",
    colour = "transparent",
    family = body_font,
    text.colour = text_col,
    width = 0.2,
    hjust = 0.5,
    halign = 0.5
  ) +
  geom_textbox(
    data = slice_max(plot_data, Height_ft),
    mapping = aes(
      x = Height_ft, y = 0.00025,
      label = glue(
        "{Name}<br>({formatC(Height_ft, big.mark=',')} ft)"
      )
    ),
    family = body_font,
    fill = "transparent",
    colour = "transparent",
    text.colour = text_col,
    width = 0.2,
    hjust = 0.5,
    halign = 0.5
  ) +
  geom_textbox(
    data = slice_max(plot_data, Height_ft),
    mapping = aes(
      x = 3020, y = 0.0002,
      label = glue(
        "<span style='font-family:\"fa_regular\";'>&#xf060;</span> 3,000 ft required for Munro classification"
      )
    ),
    fill = "transparent",
    colour = "transparent",
    family = body_font,
    text.colour = "white",
    width = 0.2,
    hjust = 0,
    halign = 0
  ) +
  geom_textbox(
    data = slice_max(plot_data, Height_ft),
    mapping = aes(
      x = mean(plot_data$Height_ft), y = y_at_mean,
      label = glue(
        "<span style='font-family:\"fa_regular\";'>&#xf060;</span> Mean height of {formatC(round(mean(plot_data$Height_ft)), big.mark=',')} ft"
      )
    ),
    fill = "transparent",
    colour = "transparent",
    family = body_font,
    text.colour = "#45465E",
    width = 0.2,
    hjust = 0,
    halign = 0.2
  ) +
  annotate(
    "segment",
    x = mean(plot_data$Height_ft),
    xend = mean(plot_data$Height_ft),
    y = 0, yend = y_at_mean,
    colour = highlight_col, linetype = "dashed"
  ) +
  geom_textbox(
    data = slice_max(plot_data, Height_ft),
    mapping = aes(
      x = median(plot_data$Height_ft), y = y_at_med,
      label = glue(
        "<span style='font-family:\"fa_regular\";'>&#xf060;</span> Median height of {formatC(round(median(plot_data$Height_ft)), big.mark=',')} ft"
      )
    ),
    fill = "transparent",
    colour = "transparent",
    family = body_font,
    text.colour = "#45465E",
    width = 0.2,
    hjust = 0,
    halign = 0.2
  ) +
  annotate(
    "segment",
    x = median(plot_data$Height_ft),
    xend = median(plot_data$Height_ft),
    y = 0, yend = y_at_med,
    colour = highlight_col, linetype = "dashed"
  ) +
  scale_x_continuous(
    limits = c(3000, 4500),
    labels = scales::label_comma()
  ) +
  scale_y_continuous(limits = c(0, 0.0018)) +
  labs(
    x = "Height (feet)", y = NULL,
    tag = cap
  ) +
  coord_cartesian(expand = FALSE) +
  theme_grey(base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 30, 10, -2.7),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag.position = c(1.02, 0.98),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      vjust = 1,
      valign = 1,
      lineheight = 1,
      family = body_font,
      maxwidth = 0.7,
      size = rel(1)
    ),
    axis.text.x = element_text(hjust = -0.1, colour = text_col),
    axis.title.x = element_text(
      hjust = 1.06, face = "bold",
      colour = text_col
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-08-19", paste0("20250819", ".png")),
  height = 5,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-08-19", paste0("20250819", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
