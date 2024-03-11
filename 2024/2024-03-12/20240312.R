# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(treemapify)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-03-12")
fiscal_sponsor_directory <- tuesdata$fiscal_sponsor_directory


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Data wrangling ----------------------------------------------------------

plot_data <- fiscal_sponsor_directory |>
  select(n_sponsored, fiscal_sponsorship_model) |>
  drop_na(n_sponsored) |>
  separate_longer_delim(fiscal_sponsorship_model, "|") |>
  mutate(fiscal_sponsorship_model = replace_na(fiscal_sponsorship_model, "Unknown")) |>
  mutate(
    fiscal_sponsorship_model =
      case_when(
        str_detect(fiscal_sponsorship_model, "Model A") ~ "Model A",
        str_detect(fiscal_sponsorship_model, "Model B") ~ "Model B",
        str_detect(fiscal_sponsorship_model, "Model C") ~ "Model C",
        str_detect(fiscal_sponsorship_model, "Model D") ~ "Model D",
        str_detect(fiscal_sponsorship_model, "Model E") ~ "Model E",
        str_detect(fiscal_sponsorship_model, "Model F") ~ "Model F",
        str_detect(fiscal_sponsorship_model, "Model L") ~ "Model L",
        str_detect(fiscal_sponsorship_model, "Other") ~ "Other",
        str_detect(fiscal_sponsorship_model, "Unknown") ~ "Unknown",
        TRUE ~ "Other"
      )
  )


# Define colours and fonts-------------------------------------------------

cols_vec <- rcartocolor::carto_pal(
  length(unique(plot_data$fiscal_sponsorship_model)) + 1, "Prism"
)[1:length(unique(plot_data$fiscal_sponsorship_model))]
names(cols_vec) <- unique(plot_data$fiscal_sponsorship_model)

bg_col <- "#fafafa"
text_col <- "black"
highlight_col <- cols_vec[1]

body_font <- "roboto"
title_font <- "robotoslab"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-03-12", "recording"),
  device = "png",
  width = 7,
  height = 6,
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
title <- glue("<br><span style='font-size: 48px; font-weight: bold; font-family:{title_font};'>Fiscal</span> <span></span> <span style='font-size: 48px; font-weight: bold; font-family:{title_font};'>Sponsors</span><br><br>")
st <- glue("<span style='font-family:{body_font};'>A fiscal sponsorship is an 
           arrangement between a not-for-profit with 501(c)(3) tax-exempt status 
           and another party without tax-exempt status. The *Fiscal Sponsorship: 
           6 Ways to Do It Right* books describes six models for fiscal sponsorships - 
           Model A, the sponsor takes the project in-house; Model B, the 
           independent contractor projec;  Model C, a preapproved 
           grant relationship; Model D, group exemption; Model E, the supporting 
           organisation; Model F — technical assistance. Model L, Single Member 
           LLC also exists. Model C is the most common approach*.<br>*Some fiscal sponsors are represented multiple times 
           in this graphic as they provide different types of relationships.</span><br><br>")
cap <- paste0(
  title, st,
  "**Data**: Fiscal Sponsor Directory<br>**Graphic**:", social
)

# Plot --------------------------------------------------------------------

ggplot(plot_data,
  mapping = aes(
    area = n_sponsored,
    fill = fiscal_sponsorship_model,
    subgroup = fiscal_sponsorship_model
  )
) +
  geom_treemap(
    mapping = aes(alpha = n_sponsored),
    layout = "fixed", colour = bg_col
  ) +
  geom_treemap_subgroup_border(layout = "fixed", colour = bg_col, size = 3) +
  geom_treemap_subgroup_text(
    layout = "fixed", place = "centre", grow = TRUE,
    alpha = 1,
    family = title_font,
    padding.x = unit(2, "mm"),
    padding.y = unit(2, "mm")
  ) +
  scale_alpha_continuous(range = c(0.3, 0.8)) +
  scale_fill_manual(values = cols_vec) +
  coord_cartesian(expand = FALSE) +
  labs(
    caption = cap
  ) +
  theme_void(base_size = 24, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(l = 8, t = 5, b = 8),
      lineheight = 0.5,
      maxwidth = 0.98,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-03-12", paste0("20240312", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
