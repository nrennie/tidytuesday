# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-03")
grants <- tuesdata$grants
grant_opportunity_details <- tuesdata$grant_opportunity_details


# Load fonts --------------------------------------------------------------

font_add_google("Roboto Slab", "robotoslab")
font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "#fafafa"
text_col <- "#555656"
highlight_col <- "#b20e10"
light_col <- "#bec0c2"


# Data wrangling ----------------------------------------------------------

grant_data <- grant_opportunity_details |>
  filter(
    eligibility_individuals,
    current_closing_date_for_applications <= lubridate::ymd("20231231")
  ) |>
  select(
    opportunity_id,
    posted_date,
    current_closing_date_for_applications
  ) |>
  mutate(posted_date = case_when(
    posted_date < lubridate::ymd("20230101") ~ lubridate::ymd("20230101"),
    TRUE ~ posted_date
  ))

plot_data <- grant_data |> 
  arrange(posted_date, current_closing_date_for_applications) |> 
  mutate(opportunity_id = factor(opportunity_id, levels = opportunity_id))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-10-03", "recording"),
  device = "png",
  width = 6,
  height = 8,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "roboto"
)
title <- "US Government Grant Opportunities"
st <- "The chart below shows grants listed on grants.gov for which individuals 
are eligible to apply for, with closing dates in 2023. Of the 22 opportunities 
meeting this criteria, 12 of them are still open for applications."
cap <- paste0(
  "**Data**: grants.gov <br> **Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_segment(
    data = plot_data,
    mapping = aes(
      x = posted_date,
      xend = current_closing_date_for_applications,
      y = opportunity_id,
      yend = opportunity_id
    ),
    linewidth = 5,
    colour = light_col
  ) +
  geom_point(
    data = filter(plot_data, posted_date == lubridate::ymd("20230101")),
    mapping = aes(
      x = posted_date,
      y = opportunity_id
    ),
    size = 4.3,
    colour = light_col
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(
      x = posted_date,
      y = opportunity_id,
      label = paste0(" Grant ID: ", opportunity_id)
    ),
    hjust = 0,
    size = 8,
    family = "roboto",
    colour = "black"
  ) +
  geom_vline(
    xintercept = lubridate::ymd(Sys.Date()),
    colour = highlight_col,
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_x_date(
    limits = lubridate::ymd(c("20221220", "20231231")),
    expand = expansion(0, 0)
  ) +
  scale_y_discrete(limits = rev) +
  theme_minimal(base_size = 30, base_family = "roboto") +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      face = "bold",
      size = 48,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = "robotoslab"
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = "roboto"
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 10),
      lineheight = 0.5,
      family = "roboto"
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-10-03", paste0("20231003", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
