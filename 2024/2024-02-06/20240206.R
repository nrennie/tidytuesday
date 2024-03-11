# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(geomtextpath)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-02-06")
heritage <- tuesdata$heritage


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
sysfonts::font_add(
  family = "Font Awesome 6",
  regular = "2024/2024-02-06/Font Awesome 6 Free-Solid-900.otf"
)
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "#00205B"
highlight_col <- "#C8102E"

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------

plot_data <- heritage |>
  mutate(perc = round(100 * (`2022` - `2004`)/`2004`)) |> 
  pivot_longer(-c(country, perc),
    values_to = "n", names_to = "year",
    names_transform = as.numeric
  ) |>
  mutate(country = factor(country, levels = c("Sweden", "Norway", "Denmark"))) |> 
  mutate(perc = glue("{perc}% <span style='font-family:\"Font Awesome 6\";'>&#xf30c;</span>"))

label_data <- heritage |> 
  group_by(country) |> 
  mutate(x = mean(c(2004, 2022)),
         y = mean(c(`2004`, `2022`)) + 0.5) |> 
  ungroup()
label_data$angle = c(16, 29, 10)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-02-06", "recording"),
  device = "png",
  width = 6,
  height = 4,
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
title <- "UNESCO World Heritage Sites"
st <- "The United Nations Educational, Scientific and Cultural Organization 
(UNESCO) seeks to encourage the identification, protection and preservation 
of cultural and natural heritage around the world considered to be of outstanding 
value to humanity. World Heritage sites belong to all the peoples of the world, 
irrespective of the territory on which they are located. The numbers of world 
heritage sites in Scandinavian countries have increased between 2004 and 2022."
cap <- paste0(
  "**Data**: UNESCO World Heritage Sites<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_area(mapping = aes(x = year, y = n, fill = country, colour = country),
            outline.type = "full",
            linewidth = 1.5) +
  geom_text(data = label_data,
            mapping = aes(x = x, y = y, label = country, angle = angle, colour = country),
            vjust = -0.2,
            size = 14,
            family = body_font,
            fontface = "bold") +
  geom_richtext(data = filter(plot_data, year == 2022),
                mapping = aes(x = 2013, y = (n/2)-1.5, label = perc, colour = country),
                label.colour = "transparent",
                fill = "transparent",
                size = 16,
                family = body_font,
                fontface = "bold"
                ) +
  facet_wrap(~country, nrow = 1) +
  labs(title = title, 
       subtitle = st,
       caption = cap) +
  scale_x_continuous(breaks = c(2004, 2022)) +
  scale_fill_manual(values = c("Denmark" = "#FFFFFF", "Norway" = "#BA0C2F", "Sweden" = "#FECC02")) +
  scale_colour_manual(values = c("Denmark" = "#C8102E", "Norway" = "#00205B", "Sweden" = "#006AA7")) +
  theme_void(base_size = 24) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
    strip.text = element_blank(),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      size = 48,
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-02-06", paste0("20240206", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
