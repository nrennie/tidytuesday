library(tidyverse)
library(lubridate)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(ggforce)
library(emojifont)

# load fonts
sysfonts::font_add(family = "Font Awesome 6 Regular",
                   regular = system.file("fonts", "fontawesome", "otfs",
                                         "Font-Awesome-6-Free-Regular-400.otf", package = "nrBrand")
)
showtext::showtext_auto()

# load data
soccer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv')

# data wrangling
home_goals <- soccer |> 
  group_by(HomeTeam) |> 
  summarise(home_goals = sum(FTHG))

away_goals <- soccer |> 
  group_by(AwayTeam) |> 
  summarise(away_goals = sum(FTAG))

plot_data <- left_join(home_goals, away_goals, by = c("HomeTeam" = "AwayTeam")) |> 
  mutate(total_goals = home_goals + away_goals) |> 
  arrange(desc(total_goals)) |> 
  mutate(HomeTeam = factor(HomeTeam, levels = HomeTeam)) |> 
  # rescale to be between -2.5 and 0
  mutate(home_goals_scale = 2.5*home_goals/max(total_goals),
         away_goals_scale = 2.5*away_goals/max(total_goals)) |> 
  mutate(id = rev(seq(-2.4, 2.4, length.out = 20)))

# start recording
gg_record(
  dir = file.path("2023", "2023-04-04", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 8, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# pitch data
pitch <- data.frame(xmin = -4, xmax = 4, ymin = -3, ymax = 3)

# params
line_col <- alpha("white", 1)

# plot
g_pitch <- ggplot() +
  geom_rect(data = pitch,
            mapping = aes(xmin = xmin,
                          xmax = xmax,
                          ymin = ymin, 
                          ymax = ymax),
            fill = "#60b922") +
  geom_rect(data = pitch,
            mapping = aes(xmin = xmin + 0.5,
                          xmax = xmax - 0.5 ,
                          ymin = ymin + 0.5, 
                          ymax = ymax - 0.5),
            colour = line_col,
            fill = "transparent",
            linewidth = 1.5) +
  geom_segment(mapping = aes(x = 0, xend = 0,
                             y = -2.5, yend = 2.5),
               colour = line_col,
               linewidth = 1.5) +
  geom_circle(mapping = aes(x0 = 0, y0 = 0, r = 0.8),
              colour = line_col,
              fill = "transparent",
              linewidth = 1.5) +
  geom_rect(data = pitch,
            mapping = aes(xmin = xmin + 0.5,
                          xmax = xmin + 1.7 ,
                          ymin = ymin + 1.3, 
                          ymax = ymax - 1.3),
            colour = line_col,
            fill = "transparent",
            linewidth = 1.5) +
  geom_rect(data = pitch,
            mapping = aes(xmin = xmin + 0.5,
                          xmax = xmin + 0.9,
                          ymin = ymin + 2.3, 
                          ymax = ymax - 2.3),
            colour = line_col,
            fill = "transparent",
            linewidth = 1) +
  geom_rect(data = pitch,
            mapping = aes(xmin = xmax - 0.5,
                          xmax = xmax - 1.7 ,
                          ymin = ymin + 1.3, 
                          ymax = ymax - 1.3),
            colour = line_col,
            fill = "transparent",
            linewidth = 1.5) +
  geom_rect(data = pitch,
            mapping = aes(xmin = xmax - 0.5,
                          xmax = xmax - 0.9,
                          ymin = ymin + 2.3, 
                          ymax = ymax - 2.3),
            colour = line_col,
            fill = "transparent",
            linewidth = 1) +
  geom_arc_bar(data = pitch,
               mapping = aes(x0 = xmin + 1.7,
                             y0 = 0,
                             r0 = 0,
                             r = 0.6,
                             start = 0,
                             end = pi),
               colour = line_col,
               fill = "transparent",
               linewidth = 1.5) +
  geom_arc_bar(data = pitch,
               mapping = aes(x0 = xmax - 1.7,
                             y0 = 0,
                             r0 = 0,
                             r = 0.6,
                             start = pi,
                             end = 2*pi),
               colour = line_col,
               fill = "transparent",
               linewidth = 1.5) +
  geom_rect(data = pitch,
            mapping = aes(xmin = xmin,
                          xmax = xmax,
                          ymin = ymin, 
                          ymax = ymax),
            fill = alpha("#60b922", 0.6)) +
  coord_cartesian(expand = FALSE) +
  theme_void()

# add data to plot
g_data <- g_pitch + 
  geom_segment(data = plot_data,
               mapping = aes(x = -3.5, xend = -3.5 + home_goals_scale,
                             y = id,
                             yend = id),
               colour = "black",
               linewidth = 0.7) +
  geom_segment(data = plot_data,
               mapping = aes(x = 3.5, xend = 3.5 - away_goals_scale,
                             y = id,
                             yend = id),
               colour = "black",
               linewidth = 0.7) +
  geom_richtext(data = plot_data,
                mapping = aes(
                  x = -3.5 + home_goals_scale + 0.05,
                  y = id,
                  label = "<span style='font-family:\"Font Awesome 6 Regular\";color:#FFFFFF;'>&#xf1e3;</span>"),
                colour = "transparent",
                fill = "transparent",
                size = 10) +
  geom_richtext(data = plot_data,
                mapping = aes(
                  x = 3.5 - away_goals_scale - 0.05,
                  y = id,
                  label = "<span style='font-family:\"Font Awesome 6 Regular\";color:#FFFFFF;'>&#xf1e3;</span>"),
                colour = "transparent",
                fill = "transparent",
                size = 10) +
  geom_text(data = plot_data,
             mapping = aes(x = -3.5 + home_goals_scale + 0.25,
                           y = id,
                           label = home_goals),
            colour = "black",
            size = 10,
            family = "Commissioner") +
  geom_text(data = plot_data,
             mapping = aes(x = 3.5 - away_goals_scale - 0.25,
                           y = id,
                           label = away_goals),
            colour = "black",
            size = 10,
            family = "Commissioner") +
  geom_text(data = plot_data,
            mapping = aes(x = 0, y = id, label = HomeTeam),
            colour = "black",
            size = 10,
            family = "Commissioner")

# add text
social <- nrBrand::social_caption(bg_colour = "#60b922",
                                  icon_colour = "black",
                                  font_colour = "black")

title <- "English Premier League 2021-2022"

g_data + 
  geom_richtext(data = data.frame(),
                mapping = aes(x = 0, y = -2.8, label = social),
                colour = "#60b922",
                fill = "transparent",
                size = 10
            ) +
  geom_text(data = data.frame(),
            mapping = aes(x = 0, y = 2.75, label = title),
            colour = "black",
            size = 20,
            fontface = "bold",
            family = "Fraunces") +
  geom_text(data = data.frame(),
            mapping = aes(x = -1.3, y = -1.5, label = "Home\nGoals"),
            colour = "black",
            lineheight = 0.4,
            family = "Fraunces",
            size = 14) +
  geom_text(data = data.frame(),
            mapping = aes(x = 1.3, y = -1.5, label = "Away\nGoals"),
            colour = "black",
            lineheight = 0.4,
            family = "Fraunces",
            size = 14)

# save gif
gg_playback(
  name = file.path("2023", "2023-04-04", "20230404.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
)
