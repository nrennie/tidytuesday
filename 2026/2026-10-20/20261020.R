# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-10-20")
ai_salary_majors <- tuesdata$ai_salary_majors
major_occupations <- tuesdata$major_occupations


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Montserrat")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Montserrat"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"


# Data wrangling ----------------------------------------------------------

ai_data <- ai_salary_majors |>
  select(
    cip4, field_of_study, broad_field_short,
    median_salary_4yr, ai_exposure, n_graduates
  ) |>
  drop_na()

plot_data |>
  mutate(
    n_ai = ai_exposure * n_graduates
  ) |>
  group_by(broad_field_short) |>
  summarise(
    mean_ai = mean(ai_exposure),
    n_ai = sum(n_ai),
  ) |>
  ungroup() |>
  arrange(desc(n_ai)) |>
  View()

plot_data <- ai_data |>
  mutate(
    category = if_else(
      broad_field_short == "Business", broad_field_short, "other"
    ),
    category = factor(category, levels = c("Business", "other"))
  ) |>
  arrange(desc(category), desc(n_graduates))


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Business majors have the most graduates across AI-exposed jobs"
st <- glue("The average AI Occupational Exposure score across the occupations that <span style='color:{highlight_col};'>**business**</span> majors typically feed into, tends to be higher. There are also a higher number of business graduates compared to other fields.<br><br><span style='font-size:10pt;'>Median salary after 4 years</span>")
cap <- source_caption(
  source = "U.S. Department of Education's College Scorecard | AI Occupational Exposure (AIOE) index of Felten, Raj and Seamans",
  note = "Each circle represents a college major, and the size of the circle the number of graduates. A higher AI occupational exposure score does not necessarily mean a job will be automated away, since AI can also assist the work rather than replace it.", graphic = social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_point_interactive(
    data = plot_data,
    mapping = aes(
      x = ai_exposure, y = median_salary_4yr,
      size = n_graduates,
      fill = category,
      colour = category,
      tooltip = field_of_study,
      data_id = cip4
    ),
    pch = 21,
    alpha = 0.8
  ) +
  # annotate AI
  geom_textbox(
    data = data.frame(
      x = c(1.49, -1.49),
      y = c(155000, 155000),
      label = c("**Higher exposure to AI \u2192**", "**\u2190 Lower exposure to AI**"),
      hjust = c(1, 0)
    ),
    mapping = aes(
      x = x, y = y, label = label,
      hjust = hjust, halign = hjust
    ),
    fill = "transparent",
    colour = text_col,
    box.colour = "transparent",
    family = body_font,
    size = 3.5
  ) +
  # annotate points
  annotate(
    "text", x = 0.44, y = 95000, label = "Nursing",
    family = body_font, size = 3.5
  ) +
  annotate(
    "text", x = -0.74, y = 123000, label = "Marine Transportation",
    family = body_font, size = 3.5
  ) +
  annotate(
    "text", x = -1.2, y = 45000, label = "Dance",
    family = body_font, size = 3.5
  ) +
  annotate(
    "text", x = -1.0, y = 33000, label = "Woodworking",
    family = body_font, size = 3.5
  ) +
  annotate(
    "text", x = 1.0, y = 18000, label = "Business Operations Support",
    family = body_font, size = 3.5
  ) +
  annotate(
    "text", x = 1.3, y = 145000, label = "Operations Research",
    family = body_font, size = 3.5,
    hjust = 1
  ) +
  # styling
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(
    limits = c(0, 160000),
    labels = scales::label_currency()
  ) +
  scale_fill_manual(values = c(highlight_col, "grey70")) +
  scale_colour_manual(values = c(highlight_col, "grey70")) +
  labs(
    x = "Average *AI Occupational Exposure* Score",
    y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 10, 10, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.title.x = element_textbox_simple(
      hjust = 1, halign = 1,
      colour = text_col,
      margin = margin(t = 3)
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
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
      margin = margin(b = 0, t = 15),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-10-20", paste0("20261020", ".png"))
)
