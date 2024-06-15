# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(gglgbtq)
library(ggstream)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-06-25")
lgbtq_movies <- tuesdata$lgbtq_movies


# Load fonts --------------------------------------------------------------

font_add_google("Grandstander")
showtext_auto()
showtext_opts(dpi = 300)


# Data wrangling ----------------------------------------------------------

plot_data <- lgbtq_movies |>
  mutate(year = year(release_date)) |>
  mutate(
    lang = case_when(
      original_language == "en" ~ "English",
      original_language == "es" ~ "Spanish",
      original_language == "fr" ~ "French",
      original_language == "de" ~ "German",
      original_language == "pt" ~ "Portuguese",
      original_language == "ja" ~ "Japanese",
      TRUE ~ "other"
    )
  ) |>
  select(year, lang) |>
  count(year, lang) |>
  filter(year >= 1980) |>
  complete(year, lang, fill = list(n = 0))

lang_order <- c("English", "Spanish", "French", "German", "Portuguese", "Japanese", "other")
plot_data$lang <- factor(plot_data$lang, levels = lang_order)


# Define colours and fonts-------------------------------------------------

col_palette <- (palette_lgbtq("philadelphia")[-1])[c(2:7, 1)]
names(col_palette) <- lang_order
bg_col <- "grey95"
text_col <- "black"
highlight_col <- col_palette[1]

body_font <- "Grandstander"
title_font <- "Grandstander"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-06-25", "recording"),
  device = "png",
  width = 8,
  height = 8,
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
title <- "LGBTQ+ Movies"
st <- glue(
  "**TidyRainbow** is a data project for the LGBTQ+ community who use the R language 
  ecosystem. The keywords 'lgbt', 'gay', 'lesbian', 'transgender', 'bisexual', 
  'intersex', 'queer', 'genderqueer', 'non-binary', 'gender', 'asexual' were used 
  to identify LGBTQ+ movies in **The Movie Database (TMDB)** - a community built movie 
  and TV database. The data was curated by Cara Cuiule (she/her).<br><br>
  The number of LGBTQ+ movies in
  <span style='color:{col_palette[[1]]}'>{names(col_palette)[[1]]}</span>, 
  <span style='color:{col_palette[[2]]}'>{names(col_palette)[[2]]}</span>,
  <span style='color:{col_palette[[3]]}'>{names(col_palette)[[3]]}</span>,
  <span style='color:{col_palette[[4]]}'>{names(col_palette)[[4]]}</span>,
  <span style='color:{col_palette[[5]]}'>{names(col_palette)[[5]]}</span>,
  <span style='color:{col_palette[[6]]}'>{names(col_palette)[[6]]}</span>, and 
  <span style='color:{col_palette[[7]]}'>{names(col_palette)[[7]]}</span> languages 
  has increased significantly since 1980.")
cap <- paste0(
  "**Data**: TidyRainbow<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = year, y = n)
) +
  # axis labels
  geom_segment(
    data = data.frame(year = seq(1980, 2020, 10)),
    mapping = aes(x = year, xend = year, y = 0, yend = -250),
    linetype = "dashed",
    alpha = 0.4,
    colour = text_col
  ) +
  geom_text(
    data = data.frame(year = seq(1980, 2020, 10)),
    mapping = aes(x = year, y = -260, label = year),
    colour = text_col,
    family = body_font,
    size = 4
  ) +
  # story 1
  annotate("segment",
           x = 1981, xend = 1981,
           y = 0, yend = 250, color = text_col
  ) +
  geom_textbox(
    data = data.frame(
      x = 1981, y = 250,
      label = glue(
        "**1981**<br>Norway becomes the first country to enact a law to prevent discrimination based on sexual orientation."
      )
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    vjust = 1,
    valign = 1,
    size = 4,
    maxwidth = 0.2,
    hjust = 0,
    halign = 0,
    box.colour = "transparent",
    fill = "transparent"
  ) +
  # story 2
  annotate("segment",
           x = 1990, xend = 1990,
           y = 0, yend = 325, color = text_col
  ) +
  geom_textbox(
    data = data.frame(
      x = 1990, y = 325,
      label = glue(
        "**1990**<br>The World Health Organization no longer defines same-sex desire as a mental illness."
      )
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    vjust = 1,
    valign = 1,
    size = 4,
    hjust = 0,
    halign = 0,
    maxwidth = 0.15,
    box.colour = "transparent",
    fill = "transparent"
  ) +
  # story 3
  annotate("segment",
           x = 1997, xend = 1997,
           y = 0, yend = 300, color = text_col
  ) +
  geom_textbox(
    data = data.frame(
      x = 1997, y = 300,
      label = glue(
        "**1997**<br> The *Journal of the Gay and Lesbian Medical Association*, the world's first peer-reviewed, multi-disciplinary journal dedicated to LGBT health is launched."
      )
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    vjust = 1,
    valign = 1,
    size = 4,
    hjust = 0,
    halign = 0,
    box.colour = "transparent",
    fill = "transparent"
  ) +
  # story 4
  annotate("segment",
           x = 2009, xend = 2009,
           y = 0, yend = 350, color = text_col
  ) +
  geom_textbox(
    data = data.frame(
      x = 2009, y = 350,
      label = glue(
        "**2009**<br> The first *International Transgender Day of Visibility* takes place."
      )
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    vjust = 1,
    valign = 1,
    size = 4,
    hjust = 0,
    halign = 0,
    box.colour = "transparent",
    fill = "transparent"
  ) +
  # stream plot
  geom_stream(aes(fill = lang),
    bw = 0.6, extra_span = 0.01
  ) +
  scale_fill_manual(values = col_palette) +
  scale_x_continuous(limits = c(1977, 2024)) +
  scale_y_continuous(limits = c(-300, 400)) +
  coord_cartesian(expand = FALSE) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  theme_void(base_size = 12, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      size = rel(2.1),
      face = "bold",
      margin = margin(l = 10, b = 5, t = 10),
      lineheight = 0.5,
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(l = 10, b = -20, t = 5),
      family = body_font,
      maxwidth = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(l = 10, b = 5, t = 10),
      family = body_font
    )
  )



# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-06-25", paste0("20240625", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
