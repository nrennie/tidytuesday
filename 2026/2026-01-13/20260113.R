# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(rnaturalearth)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-01-13")
africa <- tuesdata$africa 


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

top_10 <- africa |> 
  select(language, native_speakers) |> 
  distinct() |> 
  slice_max(native_speakers, n = 10) |> 
  mutate(label = glue("**{language}**<br>{round(native_speakers / 1000000, 1)} million native speakers"))

africa_map <- ne_countries(
  continent = "Africa",
  scale = "medium"
) |> 
  select(name_en, geometry) |> 
  mutate(
    name_en = case_when(
      name_en == "The Gambia" ~ "Gambia",
      name_en == "Democratic Republic of the Congo" ~ "Congo",
      TRUE ~ name_en
    )
  )

africa_data <- africa |> 
  filter(language %in% top_10$language) |> 
  select(language, native_speakers, country) 

# check match on country name
africa_map$name_en |> unique() |> sort()
africa_data$country |> unique() |> sort()

prep_lang_data <- function(lang) {
  af_data <- africa_data |> 
    filter(language == lang) 
  output_data <- africa_map |> 
    left_join(
      af_data, by = c("name_en" = "country")
    ) |> 
    mutate(language = lang,
           fill = if_else(is.na(native_speakers), "grey70", highlight_col)) |> 
    select(language, fill, geometry) |> 
    left_join(
      top_10, by = c("language" = "language")
    ) 
  return(output_data)
}

plot_data <- map(
  .x = top_10$language,
  .f = ~prep_lang_data(.x)
) |> 
  bind_rows() |> 
  mutate(
    language = factor(
      language, levels = top_10$language, labels = top_10$label
    )
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2026", "2026-01-13", "recording"),
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
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
title <- glue("<span style='font-family:{title_font}; font-size: 20pt;'>**Arabic is the most widely spoken language in Africa**</span>")
st <- "Top 10 most widely spoken languages in Africa by number of native speakers"
cap <- paste0(
  title, "<br>", st, "<br><br>",
  "**Data**: Wikipedia (Languages of Africa)<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = plot_data,
    mapping = aes(fill = fill),
    colour = bg_col
  ) +
  scale_fill_identity() +
  facet_wrap(~language) +
  labs(tag = cap) +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font,
      maxwidth = 0.5
    ),
    plot.tag.position = c(0.55, 0.15),
    strip.text = element_textbox_simple(
      margin = margin(t = 5, b = 5),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank()
  )
record_polaroid()


# Save gif ----------------------------------------------------------------

gg_stop_recording()
ggsave(
  filename = file.path("2026", "2026-01-13", paste0("20260113", ".png")),
  width = 7,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

