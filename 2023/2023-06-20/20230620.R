library(tidyverse)
library(lubridate)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(tsibble)
library(rcartocolor)

# load fonts
font_add_google("Ubuntu", "Ubuntu")
showtext_auto()

# read in data
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv")
places <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv")
day_parts_map <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/day_parts_map.csv")

# prep data
chosen_countries <- names(sort(table(ufo_sightings$country_code), decreasing = T)[1:9])
plot_data <- ufo_sightings |>
  filter(country_code %in% chosen_countries) |>
  select(reported_date_time, country_code, day_part) |>
  drop_na() |>
  mutate(day_part = case_when(
    str_detect(day_part, "dawn") ~ "dawn",
    str_detect(day_part, "dusk") ~ "dusk",
    TRUE ~ day_part
  )) |>
  mutate(day_part = factor(day_part,
    levels = c("dawn", "morning", "afternoon", "dusk", "night")
  )) |>
  mutate(year = year(reported_date_time)) |>
  filter(year > 2000 & year <= 2020) |>
  group_by(country_code, day_part, year) |>
  summarise(n = n()) |>
  ungroup() |>
  as_tsibble(index = year, key = c(country_code, day_part)) |>
  fill_gaps(.start = 2000, .end = 2020) |>
  mutate(n = replace_na(n, 0)) |>
  as_tibble()

# start recording
gg_record(
  dir = file.path("2023", "2023-06-20", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "#fafafa"
col_pal <- rcartocolor::carto_pal(n = 5, "Sunset")
dark_col <- "grey20"

# text
title <- "UFO Sightings"
social1 <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#b95e9a;'>&#xf099;</span><span style='color:#fafafa;'>.</span><span style='font-family:Ubuntu;color:grey20;'>@nrennie35</span><span style='color:#fafafa;'>..</span>"
social2 <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#b95e9a;'>&#xf4f6;</span><span style='color:#fafafa;'>.</span><span style='font-family:Ubuntu;color:grey20;'>fosstodon.org/@nrennie</span><span style='color:#fafafa;'>..</span>"
social3 <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#b95e9a;'>&#xf09b;</span><span style='color:#fafafa;'>.</span><span style='font-family:Ubuntu;color:grey20;'>nrennie</span><span style='color:#fafafa;'>..</span>"
st <- glue::glue("In the countries which report the highest numbers of UFO sightings 
to the National Reporting Center, the majority of sightings occur at 
<span style='color:{col_pal[5]}'>night</span>. Far fewer occur at 
<span style='color:{col_pal[4]}'>dusk</span>, in the <span style='color:{col_pal[3]}'>afternoon</span>, 
<span style='color:{col_pal[2]}'>morning</span>, or at <span style='color:{col_pal[1]}'>dawn</span>.")
tg <- paste0(
  "<span style='font-size:38pt; font-weight: bold;'>**UFO Sightings**</span><br><br>", st,
  "<br><br>**Data**: National UFO Reporting Center<br><br>",
  social1, "<br>", social2, "<br>", social3
)

# plot
ggplot(
  data = plot_data,
  mapping = aes(x = year, y = n, fill = day_part)
) +
  geom_area(position = "fill") +
  geom_text(
    mapping = aes(x = 2010, y = 0.5, label = country_code),
    colour = bg_col,
    family = "Ubuntu",
    size = 16
  ) +
  scale_x_continuous(breaks = c(2005, 2015)) +
  facet_wrap(~country_code, nrow = 3, ncol = 3) +
  scale_fill_carto_d(palette = "Sunset") +
  labs(
    x = "",
    y = "",
    tag = tg
  ) +
  theme_minimal(base_size = 36, base_family = "Ubuntu") +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    plot.tag = element_textbox_simple(
      lineheight = 0.5,
      colour = dark_col,
      margin = margin(b = 20),
      maxwidth = 0.75,
      halign = 0,
      size = 24
    ),
    plot.tag.position = c(-0.2, 0.5),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(margin = margin(t = -10)),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0, "lines"),
    plot.margin = margin(10, 15, 10, 150),
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-06-20", "20230620.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
