# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-08-12")
attribution_studies <- tuesdata$attribution_studies
attribution_studies_raw <- tuesdata$attribution_studies_raw


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "black"
highlight_col <- "#d62246"

body_font <- "Ubuntu"
title_font <- "Ubuntu"


# Data wrangling ----------------------------------------------------------

plot_data <- attribution_studies |>
  filter(study_focus == "Event") |>
  count(cb_region, event_type, classification) |>
  mutate(
    classification = factor(classification,
      levels = c("Decrease, less severe or less likely to occur", "No discernible human influence", "More severe or more likely to occur", "Insufficient data/inconclusive")
    )
  ) |> 
  mutate(
    event_type = case_when(
      event_type %in% c("Heat", "Drought", "Sunshine", "Wildfire") ~ "Heat, drought, sunshine, or wildfire",
      event_type %in% c("Storm", "Rain & flooding", "River flow") ~ "Storms, rain, flooding, or river flow",
      event_type %in% c("Impact", "Compound", "Atmosphere") ~ "Other",
      event_type == "Cold, snow & ice" ~ "Cold, snow, and ice",
      event_type == "Oceans" ~ "Ocean events",
      TRUE ~ event_type
    ),
    event_type = factor(event_type, levels = c(
      "Heat, drought, sunshine, or wildfire",
      "Storms, rain, flooding, or river flow",
      "Cold, snow, and ice",
      "Ocean events",
      "Other"
    ))
  ) |> 
  filter(!(cb_region %in% c("Global", "Northern hemisphere"))) |> 
  mutate(
    cb_region = case_when(
      cb_region %in% c("Oceania", "Australia and New Zealand") ~ "Oceania, Australia, or New Zealand ",
      TRUE ~ cb_region
    ),
    cb_region = str_wrap(cb_region, 14),
    cb_region = factor(cb_region,
                       levels = c("Northern\nAmerica",
                                  "Europe", 
                                  "Latin America\nand the\nCaribbean",
                                  "Northern\nAfrica and\nwestern Asia",
                                  "Sub-Saharan\nAfrica",
                                  "Central and\nsouthern Asia",
                                  "Eastern and\nsouth-eastern\nAsia",
                                  "Oceania,\nAustralia, or\nNew Zealand",
                                  "Antarctica",
                                  "Arctic"
                                  ))
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-08-12", "recording"),
  device = "png",
  width = 7,
  height = 7,
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
title <- "Increased frequency and severity of extreme events due to climate change"
st <- "Attribution studies calculate whether, and by how much, climate change affected the intensity, frequency or impact of extreme events. This chart shows publications which categorise whether events were <span style='color:#17bebb;'>**less severe or less likely to occur**</span> or <span style='color:#d62246;'>**more severe or more likely to occur**</span> due to climate change; or whether there was <span style='color:#946BC7;'>**no discernible human influence**</span>. Though the trends are clear, with some regions understudied and <span style='color:grey50;'>**inconclusive evidence**</span> in others, more research is needed."
cap <- paste0(
  "**Data**: Carbon Brief<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_col(
    mapping = aes(
      x = "1", y = n, fill = classification
    ),
    position = "fill"
  ) +
  annotate("rect", xmin = 0.55, xmax = 1.45, ymin = 0, ymax = 1,
           fill = "transparent", colour = text_col) +
  facet_grid(cb_region ~ event_type,
    switch = "y"
  ) +
  scale_fill_manual(
    values = c("#17bebb", "#A480CF", "#d62246", "grey50")
  ) +
  labs(
    x = NULL, y = NULL,
    title = title, subtitle = st, caption = cap
  ) +
  coord_cartesian(expand = FALSE) +
  theme_grey(base_family = body_font) +
  theme(
    legend.position = "none",
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
      lineheight = 1,
      family = title_font,
      face = "bold",
      size = rel(1.2)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 1,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      lineheight = 1,
      family = body_font
    ),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text.y.left = element_text(
      angle = 0,
      hjust = 1,
      face = "bold",
      margin = margin(r = 5),
    ),
    strip.text.x = element_textbox_simple(
      maxwidth = 1,
      height = 0.08,
      valign = 0,
      vjust = 0,
      margin = margin(b = 3),
      hjust = 0,
      face = "bold"
    ),
    panel.spacing.x = unit(0.6, "lines"),
    panel.spacing.y = unit(0.25, "lines")
  )


# Save png ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-08-12", paste0("20250812", ".png")),
  height = 7,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)


# Highlighted version -----------------------------------------------------

highlight_plot <- function(category, highlight_colour) {
  ggplot(data = plot_data) +
    geom_col(
      mapping = aes(
        x = "1", y = n, fill = (classification == category)
      ),
      position = "fill"
    ) +
    annotate("rect", xmin = 0.55, xmax = 1.45, ymin = 0, ymax = 1,
             fill = "transparent", colour = text_col) +
    facet_grid(cb_region ~ event_type,
               switch = "y"
    ) +
    scale_fill_manual(
      values = c("grey80", highlight_colour)
    ) +
    labs(
      x = NULL, y = NULL,
      title = title, caption = cap,
      subtitle = glue("Attribution studies calculate whether, and by how much, climate change affected the intensity, frequency or impact of extreme events. This chart shows publications which categorise whether events were <span style='color:{highlight_colour};'>**{str_to_lower(category)}**</span> due to climate change or <span style='color:grey30;'>**not**</span>.")
    ) +
    coord_cartesian(expand = FALSE) +
    theme_grey(base_family = body_font) +
    theme(
      legend.position = "none",
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
        lineheight = 1,
        family = title_font,
        face = "bold",
        size = rel(1.2)
      ),
      plot.subtitle = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 5, t = 5),
        lineheight = 1,
        family = body_font
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 0, t = 10),
        lineheight = 1,
        family = body_font
      ),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      strip.background = element_blank(),
      strip.text.y.left = element_text(
        angle = 0,
        hjust = 1,
        face = "bold",
        margin = margin(r = 5),
      ),
      strip.text.x = element_textbox_simple(
        maxwidth = 1,
        height = 0.08,
        valign = 0,
        vjust = 0,
        margin = margin(b = 3),
        hjust = 0,
        face = "bold"
      ),
      panel.spacing.x = unit(0.6, "lines"),
      panel.spacing.y = unit(0.25, "lines")
    )
}

highlight_plot("More severe or more likely to occur", "#d62246")
ggsave(
  filename = file.path("2025", "2025-08-12", paste0("20250812_more", ".png")),
  height = 7,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

highlight_plot("Decrease, less severe or less likely to occur", "#17bebb")
ggsave(
  filename = file.path("2025", "2025-08-12", paste0("20250812_less", ".png")),
  height = 7,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-08-12", paste0("20250812", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
