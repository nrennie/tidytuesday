library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(ggalluvial)

# load fonts
font_add_google("Ubuntu")
showtext_auto()

# load data
detectors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv")

# exploratory
prop.table(table(detectors$.pred_class))
prop.table(table(detectors$kind))
prop.table(table(filter(detectors, kind == "AI")$.pred_class))
prop.table(table(filter(detectors, kind == "Human")$.pred_class))
prop.table(table(filter(detectors, native == "Yes")$.pred_class))
prop.table(table(filter(detectors, native == "No")$.pred_class))

# data wrangling
plot_data <- detectors |>
  mutate(sub_kind = case_when(
    native == "No" ~ paste0(kind, "\n(Non-native)"),
    native == "Yes" ~ paste0(kind, " (Native)"),
    TRUE ~ kind
  )) |>
  select(kind, sub_kind, .pred_class, .pred_AI)

# reformat data
alluvial_data <- plot_data |>
  select(-.pred_AI) |>
  mutate(correct = (kind == .pred_class)) |>
  group_by(kind, sub_kind, .pred_class, correct) |>
  summarise(Freq = n()) |>
  ungroup()

# start recording
gg_record(
  dir = file.path("2023", "2023-07-18", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 9, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "#FFF9FB"
highlight_col <- "#8A3033"
light_col <- "#b9c6cd"
text_col <- "#16425B"

# text
st <- glue::glue("<span style='font-size:60px'>**Can AI detect AI?**</span><br><br>Generative AI makes it incredibly easy to generate large amounts of
content in a short space of time. But can AI also be used to detect when
content was written by AI? Liang et al. tested almost 1,000 documents across seven different GPT detectors (Crossplag, GPTZero, HFOpenAI,
OriginalityAI, Quil, Sapling, and ZeroGPT).<br><br>
When text was written by AI, it was <span style='color:{highlight_col}'>incorrectly classified</span> as human in 69% of
cases. When text was written by a human, it was only incorrectly classified as AI
18% of the time. However, that 18% isn't split evenly. Native English
speakers were only classified as AI in 3% of cases. Non-native English speakers
were classified as AI in 61% of cases.")
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#8A3033;'>&#xf099;</span><span style='color:#FFF9FB;'>.</span><span style='font-family:Ubuntu;color:#16425B;'>@nrennie35</span><span style='color:#FFF9FB;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#8A3033;'>&#xf4f6;</span><span style='color:#FFF9FB;'>.</span><span style='font-family:Ubuntu;color:#16425B;'>fosstodon.org/@nrennie</span><span style='color:#FFF9FB;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#8A3033;'>&#xf09b;</span><span style='color:#FFF9FB;'>.</span><span style='font-family:Ubuntu;color:#16425B;'>nrennie</span>"
cap <- paste0(
  "**Data**: GPT Detectors Are Biased Against Non-Native English
              Writers. arXiv: 2304.02819<br>Weixin Liang, Mert Yuksekgonul, Yining Mao, Eric Wu, James Zou.<br>**Chart**: ",
  social
)

# Alluvial plot
ggplot(
  alluvial_data,
  aes(y = Freq, axis1 = .pred_class, axis2 = sub_kind, axis3 = kind)
) +
  geom_alluvium(
    aes(fill = correct),
    width = 0.25, alpha = 0.8, colour = bg_col,
    reverse = FALSE
  ) +
  geom_stratum(width = 0.25, fill = text_col, color = bg_col, reverse = FALSE) +
  geom_text(
    stat = "stratum", reverse = FALSE, aes(label = after_stat(stratum)),
    lineheight = 0.3,
    size = 6.5,
    colour = bg_col,
    family = "Ubuntu"
  ) +
  scale_x_continuous(
    expand = c(.05, .05),
    breaks = c(1, 2, 3),
    labels = c("Classified as", "", "Written by")
  ) +
  scale_fill_manual(values = c(highlight_col, light_col)) +
  labs(
    y = NULL,
    caption = cap,
    tag = st
  ) +
  coord_flip() +
  theme_minimal(base_size = 22, base_family = "Ubuntu") +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      hjust = 1,
      colour = text_col,
      margin = margin(r = -15)
    ),
    plot.title = element_textbox_simple(
      family = "Ubuntu",
      colour = text_col,
      hjust = 0,
      size = 40,
      margin = margin(t = 20)
    ),
    plot.tag.position = c(-0.22, 0.5),
    plot.tag = element_textbox_simple(
      family = "Ubuntu",
      colour = text_col,
      lineheight = 0.5,
      maxwidth = 0.65,
      margin = margin(t = 20, b = 30)
    ),
    plot.caption = element_textbox_simple(
      family = "Ubuntu",
      colour = text_col,
      hjust = 1,
      halign = 1,
      lineheight = 0.5,
      margin = margin(t = -10, r = 20, b = 5)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(5, 0, 5, 210)
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-07-18", "20230718.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
