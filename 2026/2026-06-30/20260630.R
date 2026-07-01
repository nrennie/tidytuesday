# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggfx)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-06-30")
wreck_inventory <- tuesdata$wreck_inventory


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#051d42"
text_col <- "#eceff4"
highlight_col <- "#8498b5"


# Data wrangling ----------------------------------------------------------

plot_data <- wreck_inventory |> 
  select(year, classification) |> 
  mutate(
    year_group = (year %/% 5) * 5
  ) |> 
  drop_na() |> 
  group_by(classification) |> 
  mutate(class_size = n()) |> 
  ungroup() |> 
  mutate(
    classification = case_when(
      class_size >= 200 ~ classification,
      TRUE ~ "Other"
    )
  ) |>
  group_by(year, classification) |> 
  summarise(n = n(), .groups = "drop") |> 
  arrange(n) |> 
  filter_out(classification == "Unknown")

ship_levels <- plot_data |> 
  group_by(classification) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(n) |> 
  pull(classification)

plot_data$classification <- factor(
  plot_data$classification, levels = ship_levels, labels = paste0(rep(c("", "\n"), 5), ship_levels)
)

annot0 <- wreck_inventory |> 
  filter(year <= 1350,  classification != "Unknown")
annot1 <- wreck_inventory |> 
  filter(year <= 1400, classification == "Ship")
annot2 <- plot_data |> 
  slice_max(n) 
annot3 <- wreck_inventory |> 
  filter(year >= 1590, year <= 1610,  classification != "Unknown") |> 
  arrange(year)
annot4 <- wreck_inventory |> 
  filter(year >= 1540, year <= 1560,  classification == "Ship") |> 
  arrange(year)

annot0_text <- "**8<sup>th</sup> August 1306**<br>Merchant vessel, *Nicholas*, became wrecked during a severe storm near Portmarnock, County Dublin. Many individuals were charged with the theft of the cargo that had washed ashore."

annot1_text <- "**15<sup>th</sup> December 1379**<br>Shipwrecking of unknown vessel, lost at Scariff Island, County Kerry."

annot2_text <- "Increase in steamship (and submarine) shipwrecks during World War I and World War II."

annot3_text <- "**11<sup>th</sup> October 1601**<br>Two vessels sink near Kinsale, including a Spanish sailing ship."

annot4_text <- "**10<sup>th</sup> January 1549**<br>A ship called *Galeon* is wrecked near Wexford."


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Wrecked by wars and weather"
st <- "The Wreck Inventory of Ireland Database holds records of over 18,000 known and potential shipwreck sites in the marine and inland waterways of Ireland. Many of the shipwrecks detail either stormy weather or warfare as causes."
cap <- paste0("**Note**: Only shipwrecks with a known date and ship classification are included.<br>", source_caption(source = "Wreck Inventory of Ireland Database (WIID)", graphic = social))


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  # Data
  with_outer_glow(
    geom_point(
      mapping = aes(y = year, x = classification, size = n, alpha = n),
      fill = text_col,
      colour = bg_col,
      pch = 21,
      position = position_jitter(width = 0.15, height = 0)
    ),
    colour = highlight_col,
    sigma = 8,
    expand = 6
  ) +
  # annotations
  geom_textbox(
    data = data.frame(
      y = c(1250, 1390, 1970, 1590, 1510, 1710), x = c(9.5, 3, 8, 9.75, 6, 0.5), 
      label = c(annot0_text, annot1_text, annot2_text, annot3_text, annot4_text,
                "Increase in shipwrecks during 19<sup>th</sup> century."), 
      hjust = c(1, 0, 1, 1, 1, 0)
    ),
    mapping = aes(x = x, y = y, label = label, hjust = hjust, halign = hjust),
    colour = text_col,
    fill = alpha(bg_col, 0.7),
    vjust = 1,
    valign = 1,
    box.padding = unit(c(3, 3, 3, 3), "pt"),
    box.colour = "transparent",
    size = 3,
    maxwidth = unit(9, "lines")
  ) +
  scale_y_reverse(breaks = seq(2000, 1350, -50)) +
  scale_size(range = c(0.4, 4)) +
  scale_alpha(range = c(0.7, 1)) +
  labs(x = NULL, y = NULL,
       title = title,
       subtitle = st,
       caption = cap) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 25, 5, 5),
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
      margin = margin(b = 0, t = 0),
      family = body_font,
      width = 1.09
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font,
      width = 1.1
    ),
    axis.text.x.bottom = element_text(colour = text_col, face = "bold"),
    axis.text.y = element_text(colour = text_col),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = alpha(text_col, 0.1))
  ) +
  canvas(
    width = 4, height = 13,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-06-30", paste0("20260630", ".png"))
)
