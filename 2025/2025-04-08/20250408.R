# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(geofacet)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-04-08")
care_state <- tuesdata$care_state


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#F9F1F7"
text_col <- "#1D0C1A"
highlight_col <- "#833473"

body_font <- "Ubuntu"
title_font <- "Ubuntu"


# Data wrangling ----------------------------------------------------------

plot_data <- care_state |>
  filter(
    measure_name == "Percentage of patients who came to the emergency department with stroke symptoms who received brain scan results within 45 minutes of arrival Higher percentages are better",
    start_date == ymd("2023-04-01"),
    !(state %in% c("AS", "GU", "MP", "PR", "VI"))
  ) |>
  select(state, score) |>
  mutate(neg_score = 100 - score) |>
  pivot_longer(
    -state,
    values_to = "value",
    names_to = "score_type"
  )

avg_perc <- plot_data |> 
  filter(score_type == "score") |> 
  summarise(mean = mean(value)) |> 
  pull(mean) |> 
  round()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-04-08", "recording"),
  device = "png",
  width = 7,
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
title <- "How timely and effective is medical care in the US?"
st <- glue("<span style='color:{highlight_col};'>**Percentage of patients**</span> who came to the emergency department with stroke symptoms who received brain scan results within 45 minutes of arrival, from April 2023 to March 2024. The average across all states is {avg_perc}%.")
cap <- paste0(
  "**Data**:  Centers for Medicare and Medicaid Services<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  geom_col(
    mapping = aes(x = "1", y = score),
    fill = highlight_col
  ) +
  facet_geo(~state) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = title, subtitle = st, caption = cap) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_family = body_font, base_size = 9) +
  theme(
    plot.margin = margin(5, 10, 5, 10),
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = "#E2B6D9", colour = "#E2B6D9"),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 20, t = 5),
      family = body_font,
      maxwidth = 0.8
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 20),
      family = body_font
    ),
    aspect.ratio = 1,
    strip.text = element_blank(),
    strip.background = element_blank()
  )

record_polaroid()


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-04-08", paste0("20250408", ".png")),
  height = 6,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-04-08", paste0("20250408", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
