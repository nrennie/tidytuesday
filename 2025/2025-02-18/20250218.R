# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(geofacet)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-02-18")
agencies <- tuesdata$agencies


# Load fonts --------------------------------------------------------------

font_add_google("Rammetto One", "rammetto")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#DDF8F7"
text_col <- "#072221"
highlight_col <- "#20A39E"
na_col <- "grey60"

body_font <- "ubuntu"
title_font <- "rammetto"


# Data wrangling ----------------------------------------------------------

agency_data <- agencies |>
  select(state_abbr, agency_type, nibrs_start_date) |>
  mutate(
    agency_type = case_when(
      agency_type %in% c("City", "County") ~ agency_type,
      TRUE ~ "Other"
    ),
    nibrs_start_date = replace_na(
      nibrs_start_date, ymd("21000101")
    )
  ) |>
  select(state_abbr, nibrs_start_date)

dates <- seq(
  ymd("19850101"),
  ymd("20250101"),
  by = "5 years"
)

date_count <- function(date) {
  output1 <- agency_data |>
    filter(nibrs_start_date <= date) |>
    count(state_abbr) |>
    mutate(type = "Reporting")
  output2 <- agency_data |>
    filter(nibrs_start_date > date) |>
    count(state_abbr) |>
    mutate(type = "Not reporting")
  output <- rbind(output1, output2)
  output <- output |>
    complete(state_abbr, type, fill = list(n = 0)) |>
    mutate(date = date)
  return(output)
}

plot_data <- purrr::map(
  .x = dates,
  .f = ~ date_count(.x)
) |>
  bind_rows()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-02-18", "recording"),
  device = "png",
  width = 7,
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
title <- "Who participates in the National Incident-Based Reporting System?"
st <- glue("Currently, the FBI produces four annual publications from data provided by more than 18,000 federal, state, county, city, university and college, and tribal law enforcement agencies voluntarily participating in the UCR program. The following chart shows the percentage of agencies in each state who <span style='color:{highlight_col};'>**report**</span> to the FBI’s National Incident-Based Reporting System (NIBRS), and those who <span style='color:{na_col};'>**do not report**</span>.")
cap <- paste0(
  "**Data**: FBI Crime Data API<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = date, y = n, fill = type)
) +
  geom_area(position = "fill") +
  scale_fill_manual(
    values = c(na_col, highlight_col)
  ) +
  scale_x_date(breaks = dates[c(3, 7)], date_labels = "%Y") +
  facet_geo(~state_abbr, grid = "us_state_without_DC_grid3") +
  labs(title = title,
       subtitle = st,
       caption = cap,
       x = NULL, y = NULL) +
  coord_cartesian(expand = FALSE) +
  theme_bw(base_size = 9.5, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold"
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    strip.background = element_blank(),
    strip.text = element_text(
      family = title_font,
      colour = text_col,
      hjust = 0,
      margin = margin(b = -5)
    ),
    panel.spacing = unit(0.2, "lines")
  )
record_polaroid()


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-02-18", paste0("20250218", ".png")),
  height = 7,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-02-18", paste0("20250218", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
