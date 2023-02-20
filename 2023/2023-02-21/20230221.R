library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(ggsankey)

# load fonts
font_add_google("Fraunces", "Fraunces")
font_add_google("Commissioner", "Commissioner")
showtext_auto()

# load data
bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv")

# look up table for colours
colour_lookup <- bob_ross |>
  select(colors, color_hex) |>
  distinct() |>
  # process colours
  mutate(colors = str_remove_all(colors, "\\[")) |>
  mutate(colors = str_remove_all(colors, "\\]")) |>
  mutate(colors = str_remove_all(colors, "'")) |>
  mutate(colors = str_remove_all(colors, "\\\\n")) |>
  mutate(colors = str_remove_all(colors, "\\\\r")) |>
  # process hex
  mutate(color_hex = str_remove_all(color_hex, "\\[")) |>
  mutate(color_hex = str_remove_all(color_hex, "\\]")) |>
  mutate(color_hex = str_remove_all(color_hex, "'")) |>
  # unlist
  mutate(colors = strsplit(colors, ",")) |>
  mutate(color_hex = strsplit(color_hex, ", ")) |>
  unnest(c(colors, color_hex)) |>
  distinct() |>
  mutate(colors = str_trim(colors))

# number of times each colour used per season (avg/episode)
plot_data <- bob_ross |>
  select(c(season, Black_Gesso:Alizarin_Crimson)) |>
  group_by(season) |>
  summarise(across(Black_Gesso:Alizarin_Crimson, ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(
    cols = -season,
    names_to = "colors",
    values_to = "n"
  ) |>
  mutate(colors = str_replace_all(colors, "_", " ")) |>
  left_join(colour_lookup, by = "colors", multiple = "all")

# start recording
gg_record(
  dir = file.path("2023", "2023-02-21", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)


# subtitle
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#2F4F4F;'>&#xf099;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>@nrennie35</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#2F4F4F;'>&#xf4f6;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>fosstodon.org/@nrennie</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#2F4F4F;'>&#xf09b;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>nrennie</span><span style='color:white;'>..</span>"

# plot
ggplot(
  data = plot_data,
  mapping = aes(
    x = season,
    value = n,
    node = colors,
    fill = I(color_hex)
  )
) +
  geom_sankey_bump(
    space = 1,
    color = "transparent",
    smooth = 6,
    alpha = 0.8
  ) +
  labs(
    title = "Bob Ross Paintings",
    caption = social
  ) +
  theme_void() +
  theme(
    text = element_text(
      family = "Commissioner",
      colour = "#546666"
    ),
    plot.margin = margin(5, 10, 5, 10),
    plot.title = element_text(
      family = "Fraunces",
      size = 60,
      hjust = 0.5,
      colour = "#2F4F4F",
      margin = unit(c(0.5, 0, 0.5, 0), "cm")
    ),
    plot.caption = element_markdown(
      size = 30,
      lineheight = 0.4,
      hjust = 0.5,
      margin = unit(c(0, 0, 0.5, 0), "cm")
    ),
    plot.background = element_rect(fill = "#F0F5F5", colour = "#F0F5F5"),
    panel.background = element_rect(fill = "#F0F5F5", colour = "#F0F5F5")
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-02-21", "20230221.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#F0F5F5"
)
