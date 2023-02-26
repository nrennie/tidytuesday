library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)

# load fonts
font_add_google("Changa")
font_add_google("Zen Dots")
showtext_auto()

# load data
afrisenti <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv")
languages <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv")
language_scripts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv")
language_countries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv")
country_regions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv")

# data wrangling
prep_data <- afrisenti |>
  group_by(language_iso_code, label) |>
  summarise(n = n()) |>
  ungroup() |>
  group_by(language_iso_code) |>
  mutate(total = sum(n)) |>
  mutate(perc = round(100 * n / total, 1)) |>
  ungroup()

most_positive <- prep_data |>
  filter(label == "positive") |>
  arrange(desc(perc)) |>
  mutate(language_iso_code = factor(language_iso_code, levels = language_iso_code)) |>
  pull(language_iso_code)

plot_data <- prep_data |>
  left_join(languages, by = "language_iso_code") |>
  mutate(language_iso_code = factor(language_iso_code, levels = most_positive)) |>
  arrange(language_iso_code) |>
  mutate(language = factor(language, levels = rev(unique(language)))) |>
  select(language, label, perc)

# start recording
gg_record(
  dir = file.path("2023", "2023-02-28", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 4, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
my_cols <- c(
  "positive" = "#407e6e",
  "neutral" = "#a4a4a4",
  "negative" = "#374A67",
  "bg" = "#fcfcfc"
)

# subtitle
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#519E8A;'>&#xf099;</span><span style='color:white;'>.</span><span style='font-family:Changa;color:#2F4F4F;'>@nrennie35</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#519E8A;'>&#xf4f6;</span><span style='color:white;'>.</span><span style='font-family:Changa;color:#2F4F4F;'>fosstodon.org/@nrennie</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#519E8A;'>&#xf09b;</span><span style='color:white;'>.</span><span style='font-family:Changa;color:#2F4F4F;'>nrennie</span><span style='color:white;'>..</span>"
st <- glue("Over 100,000 tweets in 14 different African languages were analysed to uncover
the sentiment of the text. Sentiment analysis was performed and each tweet was labelled
as either <span style='color:{my_cols['positive']};'>positive</span>, 
<span style='color:{my_cols['negative']};'>negative</span>, or 
<span style='color:{my_cols['neutral']};'>neutral</span>. Nigerian
pidgin is particularly notable for its very few neutral tweets.<br><br>
Data: AfriSenti")
cap <- glue("{social}<br><br>{st}")

# plot
ggplot() +
  geom_col(
    data = plot_data,
    mapping = aes(x = perc, y = language, fill = label),
    width = 0.95
  ) +
  geom_richtext(
    data = filter(plot_data, label == "positive"),
    mapping = aes(
      x = 0.1,
      y = language,
      label = paste0("<br>", perc, "%<br>positive</i></p>")
    ),
    colour = my_cols["bg"],
    fill = NA,
    lineheight = 0.37,
    hjust = 0,
    size = 5,
    family = "Changa",
    label.color = NA
  ) +
  geom_richtext(
    data = filter(plot_data, label == "negative"),
    mapping = aes(
      x = 100,
      y = language,
      label = paste0("<p style='font-size: 20px;'>", "<b>",language,"</b>", "<br><i>", perc, "%<br>negative</i></p>")
    ),
    colour = my_cols["bg"],
    fill = NA,
    lineheight = 0.37,
    hjust = 0.96,
    size = 5,
    family = "Changa",
    label.color = NA
  ) +
  scale_fill_manual(values = my_cols) +
  labs(
    x = "",
    title = "African Language Sentiment",
    subtitle = cap
  ) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 20, base_family = "Changa",) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      family = "Zen Dots",
      size = rel(1.5),
      margin = margin(10, 5, 10, 5)
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.4,
      margin = margin(10, 5, 15, 5)
    ),
    axis.text = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(10, 14, 0, 10),
    plot.background = element_rect(fill = my_cols["bg"], colour = my_cols["bg"]),
    panel.background = element_rect(fill = my_cols["bg"], colour = my_cols["bg"])
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-02-28", "20230228.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = my_cols["bg"]
)
