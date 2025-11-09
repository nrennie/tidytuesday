# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(tidytext)
library(rcartocolor)


# Load data ---------------------------------------------------------------

holmes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-18/holmes.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#151C28"
text_col <- "#F2F4F8"
col_palette <- c("#FF007F", "#FF99CC", "#FFFFFF", "#00FFFF", "#007FFF")


# Data wrangling ----------------------------------------------------------

data(stop_words)
afinn_df <- get_sentiments("afinn")

book_data <- holmes |>
  filter(book == "A Scandal in Bohemia") |>
  drop_na() |>
  mutate(
    chap_start = str_detect(text, "CHAPTER"),
    chapter = cumsum(chap_start)
  ) |>
  group_by(chapter) |>
  mutate(chap_line_num = line_num - min(line_num) + 1) |>
  ungroup() |>
  select(chapter, line_num, chap_line_num, text) |>
  unnest_tokens(word, text)

sentiment_df <- book_data |>
  anti_join(stop_words, by = "word") |>
  inner_join(afinn_df, by = "word") |>
  distinct()

plot_data <- book_data |>
  left_join(sentiment_df, by = c("chapter", "line_num", "chap_line_num", "word")) |>
  group_by(line_num) |>
  mutate(word = if_else(row_number() != n(), paste0(word, " "), word)) |>
  ungroup() |>
  mutate(chars = str_split(word, "")) |>
  unnest_longer(chars) |>
  group_by(line_num) |>
  mutate(char_num = row_number()) |>
  ungroup() |>
  filter(chars != " ") |>
  filter(chapter != 0) |>
  mutate(chapter = paste("Chapter", chapter))

avg_sent <- plot_data |>
  group_by(word) |>
  slice_head() |>
  pull(value) |>
  mean(na.rm = TRUE) |>
  round(2)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-11-18", "recording"),
  device = "png",
  width = 7,
  height = 7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "The Sentiments of Sherlock Holmes: A Scandal in Bohemia"
st <- glue("The sentiment of words\\* in *A Scandal in Bohemia* is measured on a scale from -5 (<span style='color:{col_palette[2]}'>**very negative**</span>) to 5 (<span style='color:{col_palette[4]}'>**very positive**</span>). The average sentiment is slightly positive with a value of {avg_sent}. Many words in the text do not have a corresponding sentiment value, either because they are extremely common words like *the*, *a* or *is* or because historic nature of the text means these words are not included in modern lexicons.")
cap <- paste0(
  "**Note**: \\*Language cannot be reduced to the sum of its positive and negative scores for each word.<br>",
  "**Data**: sherlock R package<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_tile(
    data = plot_data,
    mapping = aes(x = char_num, y = chap_line_num, fill = value)
  ) +
  scale_y_reverse() +
  scale_fill_gradientn(
    colours = col_palette,
    limits = c(-5, 5), na.value = "#232F43"
  ) +
  facet_wrap(~chapter, nrow = 1) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    strip.text = element_text(
      family = title_font, margin = margin(t = 10, b = -10, l = 5),
      hjust = 0, colour = text_col
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5, l = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5, l = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 0, l = 5),
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-11-18", paste0("20251118", ".png")),
  width = 7,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-11-18", paste0("20251118", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
