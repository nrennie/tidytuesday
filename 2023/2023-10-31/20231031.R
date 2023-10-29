# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(purrr)
library(tidytext)
library(ggstream)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-31")
horror_articles <- tuesdata$horror_articles


# Load fonts --------------------------------------------------------------

font_add_google("Eater", "eater")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "grey10"
highlight_col <- "#880808"

body_font <- "ubuntu"
title_font <- "eater"


# Data wrangling ----------------------------------------------------------

data(stop_words)
common_words <- horror_articles |>
  select(claim) |>
  unnest_tokens(word, claim) |>
  anti_join(stop_words, by = "word") |>
  count(word) |>
  arrange(desc(n)) |>
  slice_head(n = 10) |>
  pull(word)

horror_words <- horror_articles |>
  mutate(year = year(published)) |>
  select(year, claim) %>%
  `[<-`(., common_words, value = 0)

for (i in common_words) {
  for (j in 1:nrow(horror_words)) {
    if (str_detect(horror_words[["claim"]][j], fixed(i, ignore_case = TRUE))) {
      horror_words[[i]][j] <- 1
    }
  }
}

horror_data <- horror_words |>
  select(-claim) |>
  group_by(year) |>
  summarise(across(killed:children, sum)) |>
  pivot_longer(-year, names_to = "word", values_to = "n") 

cols_df <- data.frame(
  word = common_words,
  colour = c("grey10", "grey20", "grey30", "#d00c0c",
             "grey40", "grey50", "#a00909", "#b80b0b",
             "grey60", "#880808")
)

plot_data <- left_join(horror_data, cols_df, by = "word") |> 
  mutate(word = factor(word, levels = common_words))

story1 <- horror_articles |> 
  filter(rating == "legend") |> 
  slice(18) |> 
  mutate(year = year(published)) |> 
  mutate(text = glue("**{title}**<br>{claim}")) |> 
  select(year, text)

story2 <- horror_articles |> 
  filter(published == "2018-09-15") |> 
  mutate(year = year(published)) |> 
  mutate(text = glue("**{title}**<br>{claim}")) |> 
  select(year, text)

story3 <- horror_articles |> 
  filter(published == "2010-11-04") |> 
  mutate(year = year(published)) |> 
  mutate(text = glue("**{title}**<br>{claim}")) |> 
  select(year, text)

story4 <- horror_articles |> 
  filter(published == "1998-09-21") |> 
  mutate(year = year(published)) |> 
  mutate(text = glue("**{title}**<br>{claim}")) |> 
  select(year, text)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-10-31", "recording"),
  device = "png",
  width = 8,
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
title <- "Horror Legends"
st <- glue::glue(
  "Of the {nrow(horror_articles)} horror articles published on Snopes.com between 
  1997 and 2023, only {sum(horror_articles$rating == 'true')} were rated as *true* stories. 
  The top 10 most frequent words in the claims were: {str_flatten_comma(common_words, last = ', and ')} - showing 
  a disproportionate number of horror descriptions featuring <span style='color:{highlight_col}'>
  women and children</span>."
  )
cap <- paste0("**Data**: Snopes.com<br>", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = year, y = n)
) +
  # axis labels
  geom_segment(
    data = data.frame(year = seq(2000, 2020, 5)),
    mapping = aes(x = year, xend = year, y = 0, yend = -15),
    linetype = "dashed",
    alpha = 0.4,
    colour = text_col
  ) +
  geom_text(
    data = data.frame(year = seq(2000, 2020, 5)),
    mapping = aes(x = year, y = -16, label = year),
    colour = text_col,
    family = body_font,
    size = 8
  ) +
  # story 1
  geom_segment(
    data = story1,
    mapping = aes(x = year, xend = year, y = 0, yend = 14),
    colour = text_col
  ) +
  geom_textbox(
    data = story1,
    mapping = aes(x = year, y = 14, label = text),
    colour = text_col,
    family = body_font,
    vjust = 0.95,
    size = 8,
    lineheight = 0.5,
    hjust = 0,
    halign = 0,
    box.colour = "transparent",
    fill = "transparent"
  ) +
  # story 2
  geom_segment(
    data = story2,
    mapping = aes(x = year, xend = year, y = 0, yend = 19),
    colour = text_col
  ) +
  geom_textbox(
    data = story2,
    mapping = aes(x = year, y = 19, label = text),
    colour = text_col,
    family = body_font,
    vjust = 0.95,
    size = 8,
    maxwidth = 0.2,
    lineheight = 0.5,
    hjust = 0,
    halign = 0,
    box.colour = "transparent",
    fill = "transparent"
  ) +
  # story 3
  geom_segment(
    data = story3,
    mapping = aes(x = year, xend = year, y = 0, yend = 12),
    colour = text_col
  ) +
  geom_textbox(
    data = story3,
    mapping = aes(x = year, y = 12, label = text),
    colour = text_col,
    family = body_font,
    vjust = 0.95,
    size = 8,
    maxwidth = 0.3,
    lineheight = 0.5,
    hjust = 0,
    halign = 0,
    box.colour = "transparent",
    fill = "transparent"
  ) +
  # story 4
  geom_segment(
    data = story4,
    mapping = aes(x = year, xend = year, y = 0, yend = 22),
    colour = text_col
  ) +
  geom_textbox(
    data = story4,
    mapping = aes(x = year, y = 22, label = text),
    colour = text_col,
    family = body_font,
    vjust = 0.95,
    size = 8,
    minwidth = 0.35,
    maxwidth = 0.35,
    lineheight = 0.5,
    hjust = 0,
    halign = 0,
    box.colour = "transparent",
    fill = "transparent"
  ) +
  # stream plot
  geom_stream(aes(fill = colour),
    bw = 0.6, extra_span = 0.001, sorting = "onset"
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_fill_identity() +
  scale_y_continuous(limits = c(-17, 22)) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 28, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      size = 50,
      margin = margin(l = 10, b = 5, t = 10),
      lineheight = 0.5,
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(l = 10, b = 15, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(l = 10, b = 5, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-10-31", paste0("20231031", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
