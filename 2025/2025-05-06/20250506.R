# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-05-06")
nsf_terminations <- tuesdata$nsf_terminations


# Functions ---------------------------------------------------------------

curved_arrow <- function(x_start, x_end, y_start, y_end, ...) {
  annotate(
    geom = "curve",
    x = x_start,
    xend = x_end,
    y = y_start,
    yend = y_end,
    linewidth = 0.3,
    color = text_col,
    arrow = arrow(
      length = unit(1.5, "mm"), type = "closed"
    ),
    ...
  )
}


# Load fonts --------------------------------------------------------------

font_add_google("Montserrat")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#FAFAFA"
text_col <- "black"
highlight_col <- "#FB5012"
null_col <- "grey30"

body_font <- "Montserrat"
title_font <- "Montserrat"


# Data wrangling ----------------------------------------------------------

plot_data <- nsf_terminations |>
  filter(termination_letter_date == ymd("20250425")) |>
  select(in_cruz_list, nsf_startdate, termination_letter_date, nsf_expected_end_date) |>
  mutate(
    days_lost = nsf_expected_end_date - termination_letter_date,
    days_done = termination_letter_date - nsf_startdate
  ) |>
  arrange(desc(days_lost), days_done) |>
  mutate(id = factor(row_number())) |>
  mutate(
    days_done = as.numeric(days_done, unit = "days"),
    days_lost = as.numeric(days_lost, unit = "days")
  )

tot_days_lost <- plot_data |>
  filter(days_lost > 0) |>
  pull(days_lost) |>
  sum()

cruz <- plot_data |>
  filter(in_cruz_list) |>
  nrow()

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-05-06", "recording"),
  device = "png",
  width = 8,
  height = 5,
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
title <- glue("{format(tot_days_lost, big.mark = ',')} days of research terminated in a single day")
st <- glue("On 25 April 2025, a total of {nrow(plot_data)} National Science Foundation grants were terminated under the Trump Administration, resulting in an expected loss of funding for {format(tot_days_lost, big.mark = ',')} days of research, on topics including flood protection, mathematics education, race in the criminal justice system, and mentorship of women in international relations. Of those grants, {cruz} were included on Ted Cruz's <span style='color:{highlight_col}'>**list of projects which he claimed *promoted Diversity, Equity, and Inclusion (DEI) or advanced neo-Marxist class warfare propaganda*.**</span>")
cap <- paste0(
  "**Data**: Grant Watch | **Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  geom_tile(
    mapping = aes(
      x = id,
      y = termination_letter_date - (days_done / 2),
      height = -days_done,
      width = 1,
      fill = in_cruz_list
    ),
    alpha = 0.4,
    linewidth = 0,
    colour = NA
  ) +
  geom_tile(
    mapping = aes(
      x = id,
      y = termination_letter_date + (days_lost / 2),
      height = days_lost,
      width = 1,
      fill = in_cruz_list
    ),
    alpha = 1,
    linewidth = 0,
    colour = NA
  ) +
  annotate("text",
    x = "500", y = ymd("20261001"),
    family = body_font,
    label = str_wrap("Research days lost", 12),
    lineheight = 1
  ) +
  curved_arrow("460", "400",
               ymd("20261001"), ymd("20260901"),
               curvature = 0.5) +
  annotate("text",
    x = "70", y = ymd("20211001"),
    family = body_font,
    label = str_wrap("Research days already carried out", 12),
    lineheight = 1
  ) +
  curved_arrow("120", "180",
               ymd("20211001"), ymd("20211101"),
               curvature = 0.5) +
  scale_y_date() +
  scale_fill_manual(values = c(null_col, highlight_col)) +
  coord_cartesian(expand = FALSE) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_family = body_font, base_size = 9) +
  theme(
    axis.text.y = element_text(margin = margin(r = 5)),
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = -75, t = 5),
      maxwidth = 0.8,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    strip.text = element_blank(),
    panel.spacing = unit(0, "lines")
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-05-06", paste0("20250506", ".png")),
  height = 5,
  width = 8,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-05-06", paste0("20250506", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
