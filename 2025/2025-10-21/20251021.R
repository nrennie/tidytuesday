# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(cowplot)
library(grid)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-10-21")
historic_station_met <- tuesdata$historic_station_met
station_meta <- tuesdata$station_meta


# Load fonts --------------------------------------------------------------

font_add_google("Libre Franklin", "libre")
font_add_google("Domine", "domine")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#F3F5F7"
text_col <- "#181D25"
highlight_col <- "#181D25"

body_font <- "libre"
title_font <- "domine"


# Data wrangling ----------------------------------------------------------

plot_data <- historic_station_met |>
  filter(year >= 1950, year <= 2020, station == "leuchars") |>
  select(station, year, month, sun) |>
  mutate(
    date = dmy(paste0("01", "-", month, "-", year)),
    end_date = ceiling_date(date, "month"),
    month = month.name[month],
    month = factor(month, levels = month.name),
    days = days_in_month(date),
    avg_sun = sun / days,
    decade = 10 * floor(year / 10),
    year_dec = year - decade
  ) |>
  drop_na()


bg_data <- data.frame(
  xmin = c(0, 2.5, 5, 7.5),
  xmax = c(2.5, 5, 7.5, 10),
  alpha = c(0.1, 0.2, 0.3, 0.4)
)

avg_data <- plot_data |> 
  group_by(month) |> 
  summarise(avg = mean(avg_sun))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-10-21", "recording"),
  device = "png",
  width = 7,
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
title <- "Looking for sunshine in Leuchars? Go in May!"
st <- "The number of hours of sunshine per day in Leuchars on the north east coast of Fife varies more in summer than winter. Each vertical line represents a different year, with bold lines indicating the 1950 - 2020 average."
cap <- paste0(
  "**Data**: Met Office<br>**Graphic**: ", social
)

plot_data |> 
  filter(month == "June") |> 
  slice_min(avg_sun)
plot_data |> 
  filter(month == "June") |> 
  slice_max(avg_sun)
plot_data |> 
  slice_min(avg_sun)

annotation1 <- "June 2007 had just 67 hours of sunshine across the whole month, averaging 2 hours 14 minutes per day."
annotation2 <- "June 1957 was the sunniest month on record having a total of 247 hours of sunshine, averaging 8 hours 14 minutes per day."
annotation3 <- "January 1996 is the least sunny month on record, with just 14 hours 18 minutes of sunshine, averaging only 28 minutes per day."

# Plot --------------------------------------------------------------------

# Plot version
p <- ggplot() +
  geom_rect(
    data = bg_data,
    mapping = aes(xmin = xmin, xmax = xmax,
                  ymin = -Inf, ymax = Inf, alpha = alpha),
    fill = "#FFBA49"
  ) +
  geom_segment(
    data = plot_data,
    mapping = aes(
      x = avg_sun, y = 0, yend = 1
    ),
    colour = alpha(highlight_col, 0.3)
  ) +
  geom_segment(
    data = avg_data,
    mapping = aes(
      x = avg, y = -0.1, yend = 1.1
    ),
    colour = highlight_col,
    linewidth = 1
  ) +
  facet_wrap(~month, ncol = 1, strip.position = "left") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_alpha_identity() +
  labs(
    x = "Average hours of sunshine per day",
    y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(
    base_family = body_font
  ) +
  theme(
    plot.margin = margin(10, 15, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.text.y = element_blank(),
    axis.title.x = element_text(hjust = 1),
    strip.text.y.left = element_text(
      hjust = 1,
      face = "bold",
      angle = 0
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
    panel.grid = element_blank(),
    panel.spacing = unit(0.2, "lines")
  )

ggdraw(p) +
  draw_text(
    x = 0.14, y = 0.37,
    size = 11,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap(annotation1, 17)
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.19, y1 = 0.48,
      x2 = 0.24, y2 = 0.51,
      curvature = -0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_text(
    x = 0.75, y = 0.25,
    size = 11,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap(annotation2, 20)
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.90, y1 = 0.37,
      x2 = 0.83, y2 = 0.51,
      curvature = 0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_text(
    x = 0.67, y = 0.76,
    size = 11,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap(annotation3, 25)
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.65, y1 = 0.76,
      x2 = 0.58, y2 = 0.83,
      curvature = 0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  )

ggsave(
  filename = file.path("2025", "2025-10-21", paste0("20251021", ".png")),
  width = 7,
  height = 8,
  bg = bg_col,
  units = "in",
  dpi = 300
)


# Art version
ggplot() +
  geom_segment(
    data = plot_data,
    mapping = aes(
      x = avg_sun, y = 0, yend = 1
    ),
    colour = alpha(highlight_col, 0.1)
  ) +
  facet_wrap(~month, ncol = 1, strip.position = "left") +
  coord_cartesian(expand = FALSE) +
  theme_void(
    base_family = body_font
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    strip.text.y.left = element_blank(),
    panel.spacing = unit(0, "lines")
  )
ggsave(
  filename = file.path("2025", "2025-10-21", paste0("20251021_art", ".png")),
  width = 7,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2025", "2025-10-21", paste0("20251021", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
