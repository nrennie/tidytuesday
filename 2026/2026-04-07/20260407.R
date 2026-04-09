# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(emojifont)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-04-07")
repairs <- tuesdata$repairs
repairs_text <- tuesdata$repairs_text


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

repair_data <- repairs |>
  mutate(repair_year = year(repair_date)) |>
  filter(
    category == "Bicycles"
  ) |>
  mutate(approx_age = repair_year - estimated_year_of_production) |>
  filter(approx_age >= 0, approx_age < 2000) |>
  select(approx_age, repaired) |>
  drop_na() |>
  mutate(age_cat = case_when(
    approx_age <= 5 ~ "Bicycles aged 5 years and under",
    approx_age > 5 & approx_age <= 10 ~ "Bicycles aged 6 to 10 years",
    approx_age > 10 & approx_age <= 20 ~ "Bicycles aged 11 to 20 years",
    approx_age > 20 ~ "Bicycles aged over 20 years"
  )) |>
  count(age_cat, repaired)

plot_data <- repair_data |> 
  mutate(n = round(n / 25)) |>
  uncount(n) |>
  mutate(
    age_cat = factor(age_cat, levels = c(
      "Bicycles aged 5 years and under",
      "Bicycles aged 6 to 10 years",
      "Bicycles aged 11 to 20 years",
      "Bicycles aged over 20 years"
    )),
    repaired = factor(repaired, levels = c("no", "half", "yes"))
  ) |>
  arrange(age_cat, repaired) |>
  group_by(age_cat) |>
  mutate(
    x = row_number() - 1,
    x = x %% 10
  ) |>
  ungroup() |>
  group_by(age_cat, x) |>
  mutate(y = row_number()) |>
  ungroup() |>
  mutate(label = fontawesome("fa-bicycle"))

perc_half <- repair_data |> 
  filter(repaired %in% c("half")) |> 
  pull(n) |> 
  sum() / sum(repair_data$n)

perc_no <- repair_data |> 
  filter(repaired %in% c("no")) |> 
  pull(n) |> 
  sum() / sum(repair_data$n)

perc_not_fully <- repair_data |> 
  filter(repaired %in% c("no", "half")) |> 
  pull(n) |> 
  sum() / sum(repair_data$n)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- glue("Only {round(100 * perc_not_fully)}% of bicycles could not be fully repaired")
st <- glue("Repair Cafe branches bring together volunteer fixers to help people learn how to repair household items that are broken. On average, {round(100 * perc_no, 1)}% of bicycles could <span style='color:#8F0700;'>**not be repaired at all**</span>, and a further {round(100 * perc_half, 1)}% could only be <span style='color:#F57200;'>**partially repaired**</span>. The age of a bicycle seems to have little effect on how difficult it is to repair.")
cap <- paste0("**Note**: Each icon represents approximately 25 bicycles.<br>"
, source_caption(source = "Repair Monitor", graphic = social, sep = " | "))


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = x, y = y, colour = repaired)
) +
  geom_text(
    mapping = aes(label = label),
    family = "fontawesome-webfont",
    size = 6
  ) +
  facet_wrap(~age_cat, ncol = 1) +
  scale_x_continuous(limits = c(-0.5, 9.5)) +
  scale_y_reverse() +
  scale_colour_manual(
    values = c("yes" = "grey", "half" = "#F57200", "no" = "#8F0700")
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = NULL, y = NULL
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(15, 15, 15, 15),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 0),
      family = title_font,
      face = "bold",
      size = rel(1.4)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 10),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text.x = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10, b = 15),
      size = rel(1.2),
      family = title_font
    ),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    axis.title.x = element_text(hjust = 0.98),
    strip.placement = "outside",
    legend.position = "none"
  ) +
  canvas(
    width = 5, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-04-07", paste0("20260407", ".png"))
)
