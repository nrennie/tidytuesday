# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-06-24")
cases_month <- tuesdata$cases_month
cases_year <- tuesdata$cases_year

# Additional data
vaccine_1 <- readxl::read_xlsx("2025/2025-06-24/data/Measles vaccination coverage 2025-22-06 21-23 UTC.xlsx")
vaccine_2 <- readxl::read_xlsx("2025/2025-06-24/data/Measles vaccination coverage dose 2 2025-22-06 21-23 UTC.xlsx")
cases_1 <- readxl::read_xlsx("2025/2025-06-24/data/Measles reported cases and incidence 2025-12-06 10-52 UTC.xlsx")


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoSlab")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "black"
highlight_col <- "#1F7A8C"

body_font <- "roboto"
title_font <- "robotoSlab"


# Data wrangling ----------------------------------------------------------

# Vaccine data
v1 <- vaccine_1 |>
  filter(COVERAGE_CATEGORY == "ADMIN") |>
  select(YEAR, COVERAGE, ANTIGEN)
v2 <- vaccine_2 |>
  filter(COVERAGE_CATEGORY == "ADMIN") |>
  select(YEAR, COVERAGE, ANTIGEN)
vaccines_data <- rbind(v1, v2) |>
  pivot_wider(names_from = ANTIGEN, values_from = COVERAGE)

# Incidence
cases_data <- cases_1 |>
  select(-(1:3)) |>
  drop_na(`2024`) |>
  pivot_longer(
    cols = everything(),
    names_to = "YEAR",
    values_to = "INCIDENCE"
  ) |>
  mutate(across(everything(), as.numeric))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-06-24", "recording"),
  device = "png",
  width = 7,
  height = 9,
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
title <- "Measles cases rising, vaccine coverage declining"
st <- "In the United Kingdom, vaccine coverage for measles has never reached the 95% target recommended by the World Health Organisation, and is currently declining further. At the same time, after a period of elimination, measles cases are once again rising."
cap <- paste0(
  "**Data**: World Health Organisation | **Graphic**:", social
)


# Plot --------------------------------------------------------------------

p1 <- ggplot(data = vaccines_data) +
  geom_area(
    mapping = aes(x = YEAR, y = MCV1),
    alpha = 0.4,
    fill = "#1F7A8C"
  ) +
  geom_area(
    mapping = aes(x = YEAR, y = MCV2),
    alpha = 0.3,
    fill = "#1F7A8C"
  ) +
  geom_line(
    mapping = aes(x = YEAR, y = MCV1),
    colour = "#1F7A8C"
  ) +
  geom_line(
    mapping = aes(x = YEAR, y = MCV2),
    colour = "#165764"
  ) +
  geom_point(
    data = filter(vaccines_data, YEAR == 2024),
    mapping = aes(x = YEAR, y = MCV1),
    colour = "#1F7A8C",
    size = 2
  ) +
  geom_point(
    data = filter(vaccines_data, YEAR == 2024),
    mapping = aes(x = YEAR, y = MCV2),
    colour = "#165764",
    size = 2
  ) +
  geom_text(
    data = filter(vaccines_data, YEAR == 2024),
    mapping = aes(x = YEAR + 0.5, y = MCV1 + 2, label = str_wrap("% receiving first dose", 12)),
    colour = "#1F7A8C",
    hjust = 0,
    vjust = 1,
    family = body_font,
    lineheight = 0.8
  ) +
  geom_text(
    data = filter(vaccines_data, YEAR == 2024),
    mapping = aes(x = YEAR + 0.5, y = MCV2 - 4, label = str_wrap("% receiving second dose", 12)),
    colour = "#165764",
    hjust = 0,
    vjust = 1,
    family = body_font,
    lineheight = 0.8
  ) +
  facet_wrap(~"Measles immunisation coverage in the United Kingdom") +
  # annotation lines
  geom_hline(yintercept = 95, colour = "#1F7A8C", linetype = "dashed") +
  annotate("segment",
    x = 1988, xend = 1988, y = 0,
    yend = filter(vaccines_data, YEAR == 1988) |> pull(MCV1), colour = "#165764"
  ) +
  annotate("segment",
    x = 1996, xend = 1996, y = 0,
    yend = filter(vaccines_data, YEAR == 1996) |> pull(MCV1), colour = "#165764"
  ) +
  annotate("segment",
    x = 1998, xend = 1998, y = 0,
    yend = filter(vaccines_data, YEAR == 1998) |> pull(MCV1), colour = "#165764"
  ) +
  # annotation text
  annotate("text",
    x = 2031, y = 99,
    label = "95% WHO recommended coverage to achieve herd immunity",
    hjust = 1, family = body_font
  ) +
  annotate("text",
    x = 1987.5, y = 50,
    label = "1988",
    hjust = 1,
    vjust = 1,
    family = body_font,
    fontface = "bold"
  ) +
  annotate("text",
    x = 1987.5, y = 45,
    label = str_wrap("Combined measles, mumps, and rubella (MMR) vaccine introduced", 12),
    hjust = 1,
    vjust = 1,
    family = body_font,
    lineheight = 0.9
  ) +
  annotate("text",
    x = 1995.5, y = 70,
    label = "1996",
    hjust = 1,
    vjust = 1,
    family = body_font,
    fontface = "bold"
  ) +
  annotate("text",
    x = 1995.5, y = 65,
    label = str_wrap("Introduction of second dose of MMR vaccine", 12),
    hjust = 1,
    vjust = 1,
    family = body_font,
    lineheight = 0.9
  ) +
  annotate("text",
    x = 1998.5, y = 30,
    label = "1998",
    hjust = 0,
    vjust = 1,
    family = body_font,
    fontface = "bold"
  ) +
  annotate("text",
    x = 1998.5, y = 25,
    label = str_wrap("Discredited paper claiming link between MMR vaccine and autism", 20),
    hjust = 0,
    vjust = 1,
    family = body_font,
    lineheight = 0.9
  ) +
  # styling
  scale_x_continuous(
    limits = c(1980, 2031),
    breaks = seq(1985, 2020, by = 5),
    minor_breaks = NULL
  ) +
  scale_y_continuous(limits = c(0, 100), minor_breaks = NULL) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_family = body_font) +
  theme(
    strip.text = element_text(
      hjust = 0, face = "bold",
      margin = margin(b = 15),
      size = rel(1.2)
    )
  )


p2 <- ggplot(data = cases_data) +
  geom_area(
    mapping = aes(x = YEAR, y = INCIDENCE),
    alpha = 0.5,
    fill = "#F06543"
  ) +
  geom_line(
    mapping = aes(x = YEAR, y = INCIDENCE),
    colour = "#BD320F"
  ) +
  geom_point(
    data = filter(cases_data, YEAR == 2024),
    mapping = aes(x = YEAR, y = INCIDENCE),
    colour = "#BD320F",
    size = 2
  ) +
  geom_text(
    data = filter(cases_data, YEAR == 2024),
    mapping = aes(
      x = YEAR + 0.5, y = INCIDENCE + 2,
      label = str_wrap("Cases per 1,000,000 people", 12)
    ),
    colour = "#BD320F",
    hjust = 0,
    vjust = 1,
    family = body_font,
    lineheight = 0.8
  ) +
  facet_wrap(~"Measles cases in the United Kingdom") +
  # annotation lines
  annotate("segment", x = 1994, xend = 1994, y = 0, yend = 700, colour = "#BD320F") +
  annotate("segment", x = 2016, xend = 2016, y = 0, yend = 500, colour = "#BD320F") +
  annotate("segment", x = 2018, xend = 2018, y = 0, yend = 300, colour = "#BD320F") +
  # annotation text
  annotate("text",
    x = 1994.5, y = 700,
    label = "1994",
    hjust = 0,
    vjust = 1,
    family = body_font,
    fontface = "bold"
  ) +
  annotate("text",
    x = 1994.5, y = 660,
    label = str_wrap("Outbreak primarily affecting school-aged children", 18),
    hjust = 0,
    vjust = 1,
    family = body_font,
    lineheight = 0.9
  ) +
  annotate("text",
    x = 2016.5, y = 500,
    label = "2016",
    hjust = 0,
    vjust = 1,
    family = body_font,
    fontface = "bold"
  ) +
  annotate("text",
    x = 2016.5, y = 460,
    label = str_wrap("Measles officially eliminated in the UK", 18),
    hjust = 0,
    vjust = 1,
    family = body_font,
    lineheight = 0.9
  ) +
  annotate("text",
    x = 2018.5, y = 300,
    label = "2018",
    hjust = 0,
    vjust = 1,
    family = body_font,
    fontface = "bold"
  ) +
  annotate("text",
    x = 2018.5, y = 260,
    label = str_wrap("Loss of elimination status due to re-established transmission", 20),
    hjust = 0,
    vjust = 1,
    family = body_font,
    lineheight = 0.9
  ) +
  # styling
  scale_x_continuous(
    limits = c(1980, 2031),
    breaks = seq(1985, 2020, by = 5),
    minor_breaks = NULL
  ) +
  scale_y_continuous(limits = c(0, 800), minor_breaks = NULL) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_family = body_font) +
  theme(
    strip.text = element_text(
      hjust = 0, face = "bold",
      margin = margin(b = 15),
      size = rel(1.2)
    )
  )

# Join plot
p1 + p2 + plot_layout(ncol = 1) +
  plot_annotation(
    title = title,
    subtitle = st,
    caption = cap
  ) &
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0),
      lineheight = 1,
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
  filename = file.path("2025", "2025-06-24", paste0("20250624", ".png")),
  height = 9,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-06-24", paste0("20250624", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
