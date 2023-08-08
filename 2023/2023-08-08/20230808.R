date_chr <- "2023-08-08"
date_strip <- stringr::str_remove_all(date_chr, "-")

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)

# load data
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/episodes.csv')
sauces <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/seasons.csv')

# load fonts
font_add_google("Nunito", "nunito")
font_add_google("Slackey", "slackey")
showtext_auto()

# colours
bg_col <- "grey10"
text_col <- "#ffce00"
highlight_col <- "#e4332f"

# prep episode data
n_episodes <- max(episodes$episode_overall)
episode_circle <- episodes |> 
  select(episode_overall, finished) |> 
  mutate(start = seq(0, 100, length.out = n_episodes+1)[1:n_episodes],
         end = seq(0, 100, length.out = n_episodes+1)[2:(n_episodes+1)],
         finished = as.numeric(finished))

# fix episode number == 999
episodes$episode_season[which(episodes$episode_season == 999)] <- 13

# prep season data
season_circle <- episodes |> 
  select(season, episode_season, finished, episode_overall) |> 
  group_by(season) |> 
  mutate(num_episodes = max(episode_season),
         perc_season_finished = sum(finished) / num_episodes,
         start_episode = min(episode_overall),
         .after = season) |> 
  ungroup() |> 
  select(season, num_episodes, start_episode, perc_season_finished) |> 
  distinct() |> 
  mutate(end_episode = start_episode + num_episodes - 1,
         start = seq(0, 100, length.out = n_episodes+1)[start_episode],
         end = seq(0, 100, length.out = n_episodes+1)[end_episode + 1]) |> 
  select(-c(start_episode, end_episode, num_episodes))

season_label <- season_circle |> 
  group_by(season) |> 
  mutate(label_x = mean(c(start, end))) |> 
  ungroup() |> 
  select(season, label_x)

# prep overall data
all_circle <- episodes |> 
  summarise(perc_all_finished = sum(finished) / n_episodes) |> 
  mutate(start = 0, 
         end = 100)
 
# start recording
gg_record(
  dir = file.path("2023", date_chr, "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# text
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "nunito"
)
st <- paste0("<br><br><br>Hot Ones is an American YouTube talk show, where celebrities 
             are interviewed over a platter of increasingly spicy chicken wings. ",
             100*all_circle$perc_all_finished, "% of guests <span style='color:#e4332f'>completed</span> 
             all of 
             the 10 sauces during their interview.",
             "<br>**Data**: Wikipedia.<br>")
title <- "Hot Ones"

# plot
ggplot() +
  # episode
  geom_rect(data = episode_circle,
            mapping = aes(xmin = start,
                          xmax = end,
                          ymin = 2,
                          ymax = 2.95,
                          alpha = finished),
            fill = highlight_col
            ) +
  # season
  geom_rect(data = season_circle,
            mapping = aes(xmin = start,
                          xmax = end,
                          ymin = 1,
                          ymax = 1.95,
                          alpha = perc_season_finished),
            fill = highlight_col
  ) +
  geom_segment(data = season_circle,
               mapping = aes(x = start,
                             xend = start,
                             y = 1,
                             yend = 1.95),
               colour = text_col
               ) +
  geom_text(data = season_label,
            mapping = aes(x = label_x, 
                          y = 1.475,
                          label = season),
            family = "nunito",
            size = 7,
            colour = text_col) +
  # all
  geom_rect(data = all_circle,
            mapping = aes(xmin = start,
                          xmax = end,
                          ymin = 0,
                          ymax = 0.95,
                          alpha = perc_all_finished),
            fill = highlight_col,
  ) +
  # label
  geom_text(data = data.frame(),
            mapping = aes(x = 0,
                          y = -1.5,
                          label = title),
            size = 20,
            colour = text_col,
            family = "slackey") +
  geom_textbox(data = data.frame(),
            mapping = aes(x = 0,
                          y = -3,
                          label = st),
            size = 8,
            family = "nunito",
            hjust = 0.5,
            halign = 0.5,
            text.colour = text_col,
            lineheight = 0.5,
            maxwidth = 6,
            fill = "transparent",
            box.colour = "transparent") +
  # styling
  scale_alpha_continuous(range = c(0, 0.7)) +
  scale_y_continuous(limits = c(-3, 2.95), expand = expansion(0, -0.5)) +
  coord_polar() +
  labs(caption = social) +
  theme_void(base_size = 30, base_family = "nunito") +
  theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col),
        legend.position = "none",
        plot.caption = element_textbox_simple(
          hjust = 0.5,
          halign = 0.5,
          colour = text_col,
          size = 24,
          lineheight = 0.5,
          family = "nunito",
          margin = margin(b = 15, t = 0)
        ))

# save gif
gg_playback(
  name = file.path("2023", date_chr, paste0(date_strip, ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
