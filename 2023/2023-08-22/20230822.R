date_chr <- "2023-08-22"
date_strip <- stringr::str_remove_all(date_chr, "-")


# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggraph)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(date_chr)
population <- tuesdata$population


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "#fafafa"
text_col <- "grey20"
plot_cols <- c("asylum" = "#000000", "idps" = "#ffdd00",
               "ooc" = "#0057b7", "refugees" = "#ababab")

# Data wrangling ----------------------------------------------------------

country <- "Ukraine"
yr <- 2011:2022

plot_data <- population |> 
  filter(coo_name == country) |> 
  select(year, refugees, asylum_seekers, idps, ooc) |> 
  group_by(year) |> 
  summarise(across(everything(), sum)) |> 
  filter(year %in% yr) |> 
  mutate(total = refugees + asylum_seekers + idps + ooc, .after = 1) |> 
  mutate(x2 = sqrt(total),
         x1 = x2 * (refugees + asylum_seekers) / total,
         x0 = 0,
         ymax = sqrt(total),
         y1a = ymax * refugees / (refugees + asylum_seekers),
         y1b = ymax * idps / (idps + ooc),
         y0 = 0
         )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", date_chr, "recording"),
  device = "png",
  width = 7,
  height = 7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = plot_cols[3],
  font_colour = text_col,
  font_family = "roboto"
)
title <- "Forcibly displaced populations from Ukraine"
st <- glue::glue(
  "The United Nations High Commissioner for Refugees (UNHCR) 
  Refugee Data Finder provides data on forcibly displaced 
  populations, including 
  <span style='color:{plot_cols['refugees']}'>refugees</span>, 
  <span style='color:{plot_cols['asylum']}'>asylum-seekers</span>, 
  <span style='color:{plot_cols['idps']}'>internally displaced people</span>, 
  and <span style='color:{plot_cols['ooc']}'>others of concern</span> over a 
  span of more than 70 years. Data from 2011 onwards shows an increase in the 
  number of forcibly displaced people originating from Ukraine, with a 
  significant increase in the number of refugees in 2022.")


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_rect(
    mapping = aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1a,
                  fill = "refugees"),
    colour = bg_col,
    linewidth = 0.2
  ) +
  geom_rect(
    mapping = aes(xmin = x0, xmax = x1, ymin = y1a, ymax = ymax,
                  fill = "asylum"),
    colour = bg_col,
    linewidth = 0.2
  ) +
  geom_rect(
    mapping = aes(xmin = x1, xmax = x2, ymin = y0, ymax = y1b,
                  fill = "idps"),
    colour = bg_col,
    linewidth = 0.2
  ) +
  geom_rect(
    mapping = aes(xmin = x1, xmax = x2, ymin = y1b, ymax = ymax,
                  fill = "ooc"),
    colour = bg_col,
    linewidth = 0.2
  ) +
  scale_fill_manual(values = plot_cols) +
  coord_fixed(expand = FALSE) +
  facet_wrap(~year, scales = "fixed", strip.position = "bottom", nrow = 3) +
  labs(title = title,
       subtitle = st,
       caption = social) +
  theme_void(base_size = 28, base_family = "roboto") +
  theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col),
        plot.margin = margin(15, 15, 15, 15),
        strip.text.x = element_text(hjust = 0, margin = margin(t = 5)),
        legend.position = "none",
        plot.title = element_textbox_simple(
          hjust = 0.5,
          halign = 0.5,
          colour = text_col,
          size = 48,
          face = "bold",
          lineheight = 0.5,
          family = "robotoslab",
          margin = margin(b = 5, t = 5)
        ),
        plot.subtitle = element_textbox_simple(
          hjust = 0.5,
          halign = 0.5,
          colour = text_col,
          size = 28,
          lineheight = 0.5,
          family = "roboto",
          margin = margin(b = -20, t = 15)
        ),
        plot.caption = element_textbox_simple(
          hjust = 0.5,
          halign = 0.5,
          colour = text_col,
          size = 20,
          lineheight = 0.5,
          family = "roboto",
          margin = margin(b = 0, t = 20)
        ))

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", date_chr, paste0(date_strip, ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
