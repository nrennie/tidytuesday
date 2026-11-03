# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(rnaturalearth)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-11-03")
grambank_languages <- tuesdata$grambank_languages
grambank_parameters <- tuesdata$grambank_parameters
grambank_values <- tuesdata$grambank_values

world <- ne_countries(scale = "medium", returnclass = "sf") |> 
  filter(sovereignt != "Antarctica")

# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add("Poppins",
         regular = "fonts/Poppins/Poppins-Regular.ttf",
         italic = "fonts/Poppins/Poppins-Italic.ttf",
         bold = "fonts/Poppins/Poppins-Bold.ttf",
         bolditalic = "fonts/Poppins/Poppins-BoldItalic.ttf"
)
font_add("FA",
         regular = "fonts/Font Awesome 6 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Poppins"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"


# Data wrangling ----------------------------------------------------------

param_choice <- grambank_parameters |>
  filter(str_detect(feature_name, "gender")) |>
  filter(str_detect(feature_name, "pronoun")) 

plot_data <- grambank_values |>
  filter(parameter_id == param_choice$parameter_id) |>
  select(language_id, value) |>
  left_join(
    grambank_languages,
    by = "language_id"
  ) |> 
  drop_na()


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)

icon_y <- glue("<span style='font-size:8pt;font-family:\"FA\";color:{highlight_col};'>&#xf219;</span>")
icon_n <- glue("<span style='font-size:8pt;font-family:\"FA\";color:grey40;'>&#xf111;</span> ")

title <- "Is there a gender distinction in independent 3<sup>rd</sup> person pronouns?"
st <- glue("Independent 3<sup>rd</sup> person pronouns are pronouns used to refer to someone or something other than the speaker (*I*) or listener (*you*), functioning independently without needing to be directly attached to a verb or verb phrase. In English, examples include *she*, *her*, *he*, *him*, *they*, and *them*.<br><br>Of the {formatC(nrow(plot_data), big.mark=',')} languages tested in the Grambank database, {sum(plot_data$value)} ({round(100*sum(plot_data$value)/nrow(plot_data))}%) have gender distinctions in independent 3<sup>rd</sup> person pronouns. The map below shows where these are <span style='color:{highlight_col}'>**present**</span> {icon_y} or <span style='color:grey40'>**absent**</span> {icon_n} in languages. European languages have a high percentage of languages with gender distinction.")
cap <- source_caption(source = "Grambank. Hedvig Skirgård et al. 2023. DOI:10.1126/sciadv.adg6175", graphic = social)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(data = world, fill = "grey85", colour = bg_col) +
  geom_point(
    data = plot_data,
    mapping = aes(x = longitude, y = latitude, 
                  shape = as.character(value)),
    colour = bg_col,
    fill = bg_col,
    size = 1
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(x = longitude, y = latitude, 
                  colour = as.character(value),
                  fill = as.character(value),
                  shape = as.character(value)),
    size = 1,
    alpha = 0.6
  ) +
  scale_shape_manual(
    values = c("1" = 18, "0" = 16)
  ) +
  scale_fill_manual(
    values = c("1" = highlight_col, "0" = "grey40")
  ) +
  scale_colour_manual(
    values = c("1" = highlight_col, "0" = "grey40")
  ) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 7.5, 5, 7.5),
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
      margin = margin(b = 5, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank()
  ) +
  canvas(
    width = 7, height = 5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-11-03", paste0("20261103", ".png"))
)
