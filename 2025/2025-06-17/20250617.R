# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(treemapify)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-06-17")
api_categories <- tuesdata$api_categories
api_info <- tuesdata$api_info
api_logos <- tuesdata$api_logos
api_origins <- tuesdata$api_origins
apisguru_apis <- tuesdata$apisguru_apis


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "grey90"
text_col <- "black"
highlight_col <- "#FF9900"

body_font <- "roboto"
title_font <- "roboto"


# Data wrangling ----------------------------------------------------------

plot_data <- api_info |>
  select(provider_name) |>
  mutate(
    provider = case_when(
      provider_name == "azure.com" ~ "Microsoft Azure",
      provider_name == "googleapis.com" ~ "Google APIs",
      provider_name == "amazonaws.com" ~ "Amazon AWS",
      provider_name == "apisetu.gov.in" ~ "API Setu",
      TRUE ~ "Other"
    )
  ) |>
  group_by(provider_name) |> 
  mutate(n = n()) |>
  ungroup() |> 
  distinct()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-06-17", "recording"),
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
title <- "<span style='font-size:28px;'>**Who provides our APIs?**</span>"
st <- "An API (Application Programming Interface) is a set of rules that allows different software systems to communicate with each other. APIs.guru's goal is to create a machine-readable Wikipedia for Web APIs in the OpenAPI Specification format.<br><br><span style='color:#0078D4'>**Microsoft**</span>, <span style='color:#34A853'>**Google**</span>, and <span style='color:#FF9900'>**Amazon**</span> dominate the landscape of API providers, alongside API Setu (who primarily provide open data APIs)."
cap <- paste0(
  title, "<br><br>", st, "<br><br>**Data**: apis.guru<br>**Graphic**:", social
)



# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(
    area = n, fill = provider,
    group = provider_name, label = provider,
    subgroup = provider
  )
) +
  geom_treemap(colour = bg_col, size = 1, layout = "fixed") +
  geom_treemap_subgroup_border(colour = bg_col, size = 3, layout = "fixed") +
  geom_treemap_subgroup_text(
    place = "centre", grow = TRUE,
    alpha = 0.8, colour = text_col,
    fontface = "italic",
    family = body_font, layout = "fixed"
  ) +
  scale_fill_manual(values = c("#FF9900", "#F36F21", "#34A853", "#0078D4", "grey70")) +
  labs(tag = cap) +
  theme_void(base_size = 12, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 250),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.85
    ),
    plot.tag.position = c(-0.73, 0.5)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-06-17", paste0("20250617", ".png")),
  height = 5,
  width = 8,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-06-17", paste0("20250617", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
