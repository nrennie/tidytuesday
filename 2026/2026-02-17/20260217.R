# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-02-17")
dataset <- tuesdata$dataset


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"


# Data wrangling ----------------------------------------------------------

plot_data <- dataset |>
  filter(measure %in% c("Total Area of Farms", "Total Sheep")) |>
  select(year_ended_june, measure, value) |>
  pivot_wider(names_from = "measure", values_from = "value") |>
  mutate(sheep_per_hectare = `Total Sheep` / `Total Area of Farms`) |>
  select(year = year_ended_june, sheep_per_hectare) |>
  drop_na()


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Sheep density decreasing since late 1980s"
st <- "Average number of sheep per hectare of total farm land in New Zealand"
cap <- paste0("**Data**: StatsNZ | **Icon**: flaticon.com | **Graphic**: ", social)

range_annot0 <- "**World War II**"

annot0 <- "**Animals Protection Act 1960**<br>Broadens cruelty offence definitions but without specific farm animal welfare standards."
annot0 <- "**Animals Protection Act 1960**<br>Broadens cruelty offence definitions but without specific farm animal welfare standards."
annot1 <- "**Animal Welfare Act 1999**<br>Introduces a duty of care for people in charge of animals, including livestock like sheep."
annot2 <- "**2003**<br>Last export of sheep for slaughter."
annot3 <- "**Animal Welfare Amendment<br>Act 2022**<br>Permanently<br>bans the export<br>of livestock by<br>sea, including<br>sheep."


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = year, y = sheep_per_hectare)
) +
  geom_line(linewidth = 1) +
  geom_textbox(
    data = data.frame(x = c(1960, 1999, 2003, 2022, 1945),
                      y = c(2.3, 3.1, 1.8, 2.3, 3.3),
                      label = c(annot0, annot1, annot2, annot3, range_annot0),
                      hjust = c(0, 0, 1, 0, 0)),
    mapping = aes(x = x, y = y, label = label, 
                  hjust = hjust, halign = hjust),
    family = body_font,
    fill = "transparent",
    box.colour = "transparent"
  ) +
  annotate("rect", xmin = 1939, xmax = 1945, ymin = -Inf, ymax = Inf,
           fill = text_col, alpha = 0.2) +
  geom_segment(
    data = data.frame(x = c(1960, 1999, 2003, 2022),
                      y = c(2.0, 3.38, 1.65, 2.73),
                      yend = c(2.65, 2.7, 2.56, 1.91)),
    mapping = aes(x = x, y = y, yend = yend)
  ) +
  scale_x_continuous(limits = c(1935, 2035),
                     breaks = seq(1935, 2025, by = 30)) +
  labs(x = NULL, y = NULL,
       title = title, subtitle = st, caption = cap,
       tag = "<img src='2026/2026-02-17/sheep.png' width='40'>") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0),
      family = body_font
    ),
    plot.tag = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5
    ),
    plot.tag.position = c(1.43, 0.95),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  canvas(
    width = 7, height = 5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-02-17", paste0("20260217", ".png"))
)
