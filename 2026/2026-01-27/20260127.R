# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(scales)
library(ggridges)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-01-27")
companies <- tuesdata$companies
legal_nature <- tuesdata$legal_nature
qualifications <- tuesdata$qualifications
size <- tuesdata$size


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
highlight_col <- "#7F055F"


# Data wrangling ----------------------------------------------------------

plot_data <- companies |>
  select(legal_nature, owner_qualification, company_size, capital_stock) |>
  pivot_longer(-capital_stock, names_to = "group", values_to = "category") |>
  mutate(
    group = str_replace(group, "_", " "),
    group = str_to_sentence(group),
    category = str_remove_all(category, "\\(.*?\\)"),
    category = str_trim(category),
    category = str_replace(category, "-", " "),
    category = str_to_sentence(category),
    category = str_replace(category, "llc", "LLC"),
    category = str_replace(category, "brazil", "Brazil")
  ) |>
  mutate(capital_stock = capital_stock / 1000000000) |> 
  group_by(group, category) |> 
  mutate(n = n()) |> 
  ungroup() |> 
  filter(n >= 5) |> 
  group_by(group) |>
  mutate(y = n_distinct(category)) |>
  ungroup()

summary_data <- plot_data |> 
  group_by(group, category) |> 
  summarise(med = median(capital_stock)) |> 
  ungroup() |> 
  arrange(desc(med))

plot_data$category <- factor(plot_data$category, levels = summary_data$category)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Publicly traded corporations report highest share capital"
st <- "Cadastro Nacional da Pessoa Jurídica. (National Registry of Legal Entities). December 2025."
cap <- paste0(
  "**Note**: Only categories with 5 or more companies are included.<br>**Data**: Cadastro Nacional da Pessoa Jurídica<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_density_ridges(
    data = plot_data,
    mapping = aes(x = capital_stock, y = category),
    fill = highlight_col,
    colour = highlight_col,
    alpha = 0.6
  ) +
  geom_text(
    data = plot_data |> group_by(category) |> slice_head(),
    mapping = aes(
      x = 0.00002,
      y = category,
      label = str_wrap(category, 28)
    ),
    alpha = 0.8,
    hjust = 1,
    family = body_font,
    size.unit = "pt",
    lineheight = 0.75,
    size = 7.5
  ) +
  geom_text(
    data = plot_data |> group_by(group) |> slice_head(),
    mapping = aes(
      x = 0.00002,
      y = y + 1,
      label = str_wrap(group, 45)
    ),
    alpha = 0.9,
    hjust = 1,
    family = body_font,
    size.unit = "pt",
    size = 10,
    fontface = "bold"
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_log10(limits = c(0.00000001, 1000),
                breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000),
                labels = function(x) format(x, scientific = FALSE, drop0trailing = TRUE)) +
  scale_y_discrete(limits = rev) +
  labs(
    x = "Declared share capital (billions, BRL)", y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
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
      margin = margin(b = 0, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.4)
    ),
    plot.subtitle = element_textbox_simple(
      colour = alpha(text_col, 0.7),
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text = element_blank(),
    strip.background.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing.y = unit(2.5, "lines"),
    axis.title.x = element_text(hjust = 1.05, margin = margin(t = 5)),
    axis.text.y = element_blank()
  ) +
  canvas(width = 5, height = 10, units = "in", bg = bg_col) -> p


# Save --------------------------------------------------------------------

save_ggplot(p, file.path("2026", "2026-01-27", paste0("20260127", ".png")))


