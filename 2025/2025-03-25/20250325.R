# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(geomtextpath)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-03-25")
report_words_clean <- tuesdata$report_words_clean


# Load fonts --------------------------------------------------------------

font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "#242f41"
highlight_col <- "#fca204"

body_font <- "lato"
title_font <- "lato"


# Data wrangling ----------------------------------------------------------

plot_data <- report_words_clean |>
  filter(year %in% c(2005, 2020)) |>
  count(year, word) |>
  filter(word %in% c("data", "websites")) |>
  mutate(
    n_label = glue("{n} uses in {year}"),
    word = str_to_title(word)
  ) |>
  mutate(
    dir_label = c("Up", "Down", "Up", "Down")
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-03-25", "recording"),
  device = "png",
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
title <- "<span style='font-size:24pt'>**Amazon's Annual Reports**</span><br><br>"
st <- "Between 2005 and 2020, mentions of the word **data** in Amazon's annual report, which summarises the company’s performance over the past year, have more than doubled. Meanwhile, mentions of **websites** have decreased dramatically.<br><br>"
cap <- paste0(
  title, st, "**Data**: ir.aboutamazon.com<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  geom_area(
    mapping = aes(x = year, y = n, fill = word)
  ) +
  geom_textpath(
    mapping = aes(x = year, y = n, label = word, colour = word),
    text_only = TRUE,
    family = body_font,
    vjust = 0,
    size = 8,
    fontface = "bold"
  ) +
  geom_text(
    data = filter(plot_data, year == 2005),
    mapping = aes(
      x = mean(c(2005, 2020)), y = 5, label = dir_label,
      colour = dir_label
    ),
    family = body_font,
    size = 8,
    fontface = "bold"
  ) +
  geom_textbox(
    mapping = aes(x = year, y = n, label = n_label,
                  hjust = if_else(
                    year == 2005, 1, 0
                  ),
                  halign = if_else(
                    year == 2005, 1, 0
                  )),
    family = body_font,
    vjust = 1,
    valign = 1,
    box.padding = unit(c(2, 2, 2, 2), "pt"),
    fill = "transparent",
    box.colour = "transparent",
    maxwidth = 0.15,
    size = 3
    
  ) +
  facet_wrap(~word) +
  scale_colour_manual(
    values = c(
      "Data" = highlight_col, "Websites" = text_col,
      "Up" = text_col,
      "Down" = highlight_col
    )
  ) +
  scale_fill_manual(
    values = c(highlight_col, text_col)
  ) +
  labs(tag = cap) +
  coord_cartesian(clip = "off") +
  theme_void(base_family = body_font, base_size = 8) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 25, 5, 175),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.tag.position = c(-0.25, 0.5),
    strip.text = element_blank(),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      family = body_font,
      maxwidth = 0.55
    ),
    panel.spacing = unit(0.8, "lines")
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-03-25", paste0("20250325", ".png")),
  height = 5,
  width = 8,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-03-25", paste0("20250325", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
