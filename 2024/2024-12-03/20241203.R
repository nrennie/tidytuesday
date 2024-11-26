# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(hms)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-12-03")
A64_traffic <- tuesdata$A64_traffic


# Load fonts --------------------------------------------------------------

font_add_google("Parkinsans", db_cache = FALSE)
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#061746"
text_col <- "white"
highlight_col <- "#1d91c0"

body_font <- "Parkinsans"


# Data wrangling ----------------------------------------------------------

all_times <- data.frame(TimeGroup = seq(
  from = as.POSIXct("2021-05-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  to = as.POSIXct("2021-05-31 23:50:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  by = "15 min"
)) |>
  mutate(TimeGroup = as_hms(TimeGroup))

plot_data <- A64_traffic |>
  mutate(
    DayOfMonth = lubridate::mday(`Report Date`), .after = `Report Date`,
    TimeGroup = trunc_hms(as_hms(`Time Period Ending`), 60 * 15)
  ) |>
  right_join(all_times, by = "TimeGroup", relationship = "many-to-many") |>
  select(SiteId, DayOfMonth, TimeGroup, `Total Volume`, `Avg mph`) |>
  mutate(
    SiteId = paste0("Site ", SiteId)
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-12-03", "recording"),
  device = "png",
  width = 8,
  height = 9,
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
title <- "When do vehicles use the A64 Eastbound?"
st <- "National Highways operates and maintains motorways and major A roads in England. Data collected from four road sensors located on the A64 (Eastbound from York to Scarborough) includes the number of vehicles travelling throughout the day and their average speed at each site. For data collected during **May 2021**, the number of vehicles isn't necessarily indicative of the speed of those vehicles."
cap <- paste0(
  "**Data**: National Highways WebTRIS API<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

blank_string <- function(string) {
  stringr::str_replace(string, string, "")
}

g1 <- ggplot(
  data = plot_data
) +
  geom_raster(
    mapping = aes(
      x = TimeGroup, y = DayOfMonth, fill = `Total Volume`
    )
  ) +
  facet_wrap(~SiteId, nrow = 1) +
  scale_x_time(breaks = NULL) +
  scale_y_reverse(name = "Day") +
  scale_fill_distiller(
    name = "Number\nof\nvehicles",
    palette = "YlGn",
    direction = 1,
    na.value = bg_col
  ) +
  guides(fill = guide_colourbar(barheight = 12)) +
  coord_cartesian(
    expand = FALSE
  )

g2 <- ggplot(
  data = plot_data
) +
  geom_raster(
    mapping = aes(
      x = TimeGroup, y = DayOfMonth, fill = `Avg mph`
    )
  ) +
  facet_wrap(~SiteId, nrow = 1, labeller = labeller(SiteId = blank_string)) +
  scale_x_time(
    breaks = as_hms(c("00:00:00", "08:00:00", "16:00:00")),
    labels = c("00:00", "08:00", "16:00")
  ) +
  scale_y_reverse(name = "Day") +
  scale_fill_distiller(
    name = "Average\nspeed\n(mph)",
    limits = c(0, 75),
    palette = "YlOrRd",
    direction = 1,
    na.value = bg_col
  ) +
  guides(fill = guide_colourbar(barheight = 12)) +
  coord_cartesian(
    expand = FALSE
  )

g1 + g2 +
  plot_layout(ncol = 1) +
  plot_annotation(
    title = title,
    subtitle = st,
    caption = cap
  ) &
  theme_minimal(
    base_size = 12, base_family = body_font
  ) +
    theme(
      text = element_text(colour = text_col),
      axis.ticks = element_blank(),
      axis.text = element_text(
        colour = text_col
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_text(
        angle = 0,
        margin = margin(r = -10)
      ),
      strip.text = element_text(
        colour = text_col
      ),
      plot.margin = margin(5, 5, 5, 5),
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 10, t = 5),
        lineheight = 0.5,
        face = "bold",
        size = rel(1.5),
        family = body_font
      ),
      plot.subtitle = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 5, t = 0),
        lineheight = 0.5,
        family = body_font
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 5, t = 5),
        lineheight = 0.5,
        family = body_font
      )
    )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2024", "2024-12-03", paste0("20241203", ".png")),
  width = 8,
  height = 9
)

gg_playback(
  name = file.path("2024", "2024-12-03", paste0("20241203", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
