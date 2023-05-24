library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(ggimage)
library(charlatan)

# load fonts
font_add_google("Roboto", "Roboto")
font_add_google("Roboto Slab", "Roboto Slab")
showtext_auto()

# read in data
squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')

# name the squirrels
num_unique <- length(unique(squirrel_data$`Unique Squirrel ID`))
set.seed(12345)
x <- PersonProvider$new()
sq_names <- replicate(n = 4500, x$first_name())
length(unique(sq_names))
sq_names <- sq_names[1:num_unique]
squirrel_names <- tibble(
  `Unique Squirrel ID` = unique(squirrel_data$`Unique Squirrel ID`),
  name = sq_names
)

# prep data
all_five <- squirrel_data |> 
  left_join(squirrel_names, by = "Unique Squirrel ID") |> 
  select(
    c(name, Running, Chasing, Climbing, Eating, Foraging)
    ) |> 
  filter(Running, Chasing, Climbing, Eating, Foraging) |> 
  pull(name)

plot_data <- squirrel_data |> 
  left_join(squirrel_names, by = "Unique Squirrel ID") |> 
  select(
    c(name, Running, Chasing, Climbing, Eating, Foraging)
  ) |> 
  pivot_longer(c(Running, Chasing, Climbing, Eating, Foraging),
               names_to = "activity",
               values_to = "is_true") |> 
  group_by(activity) |> 
  summarise(total = sum(is_true)) |> 
  mutate(total = 100 * total / num_unique) |> 
  mutate(activity = paste0(" ", activity, " "))

# define coords
# https://stackoverflow.com/questions/36579767/add-unit-labels-to-radar-plot-and-remove-outer-ring-ggplot2-spider-web-plot-co
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  
  #dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }
  
  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }
  
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          render_bg = function(self, scale_details, theme) {
            scale_details <- rename_data(self, scale_details)
            
            theta <- if (length(scale_details$theta.major) > 0)
              theta_rescale(self, scale_details$theta.major, scale_details)
            thetamin <- if (length(scale_details$theta.minor) > 0)
              theta_rescale(self, scale_details$theta.minor, scale_details)
            thetafine <- seq(0, 2 * pi, length.out = 100)
            
            rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
            
            # This gets the proper theme element for theta and r grid lines:
            #   panel.grid.major.x or .y
            majortheta <- paste("panel.grid.major.", self$theta, sep = "")
            minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
            majorr     <- paste("panel.grid.major.", self$r,     sep = "")
            
            ggplot2:::ggname("grill", grid::grobTree(
              ggplot2:::element_render(theme, "panel.background"),
              if (length(theta) > 0) ggplot2:::element_render(
                theme, majortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                id.lengths = rep(2, length(theta)),
                default.units = "native"
              ),
              if (length(thetamin) > 0) ggplot2:::element_render(
                theme, minortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                id.lengths = rep(2, length(thetamin)),
                default.units = "native"
              ),
              
              ggplot2:::element_render(
                theme, majorr, name = "radius",
                x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                id.lengths = rep(length(thetafine), length(rfine)),
                default.units = "native"
              )
            ))
          })
}

# start recording
gg_record(
  dir = file.path("2023", "2023-05-23", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 8, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "#fafafa"
dark_col <- "#555656"
highlight_col <- "#b20e10"

# text
title <- "How do New York City squirrels spend their time?"
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = dark_col,
  font_family = "Roboto"
)
st <- "Data from the 2018 Central Park Squirrel Census shows that squirrels
were often sighted either running, chasing, climbing, eating, or foraging. 
Two squirrels in the census were recorded doing all five activities at once!"
cap <- paste0("**Data**: 2018 Central Park Squirrel Census<br>
              **Image**: BirdPhotos.com (Wikimedia Commons CCA 3.0)<br>", social)

# plot
ggplot(data = plot_data,
       mapping = aes(x = activity,
                     y = total,
                     group = "1")) +
  # radar plot bg elements
  geom_line(data = data.frame(
    x = rep(plot_data$activity, 2),
                              
    y = c(rep(0, length(plot_data$activity)), rep(60, length(plot_data$activity)))),
            mapping = aes(x = x,
                          y = y,
                          group = x),
            colour = dark_col,
            alpha = 0.5) +
  geom_point(data = data.frame(
    x=plot_data$activity, y=rep(60, length(plot_data$activity))),
             inherit.aes = FALSE, 
             mapping = aes(x = x,
                           y = y),
             colour = dark_col,
             size = 4) +
  # radar plot data
  geom_polygon(fill = highlight_col,
               colour = highlight_col, 
               alpha = 0.4) +
  geom_point(colour = highlight_col,
             size = 4) +
  # add image
  geom_image(data = slice_head(plot_data, n = 1),
             aes(x = 1,
                 y = -10,
                 image = "squirrel.png"),
             size = 0.12) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  scale_y_continuous(limits = c(-10, 65),
                     breaks = c(0, 20, 40, 60))+
  coord_radar() +
  theme_minimal(
    base_size = 36,
    base_family = "Roboto"
  ) +
  theme(
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    plot.title = element_textbox_simple(
      family = "Roboto Slab",
      face = "bold",
      margin = margin(t = 20, b = 30),
      colour = dark_col,
      size = 60,
      hjust = 0,
      halign = 0,
      lineheight = 0.4,
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.5,
      colour = dark_col,
      margin = margin(b = 0),
      hjust = -0.5,
      halign = 0
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.55,
      margin = margin(t = 0),
      colour = dark_col,
      hjust = 0,
      halign = 0
    ),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10),
                               family = "Roboto Slab",
                               size = 30,
                               hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha(dark_col, 0.5),
      linewidth = 0.5,
    ),
    plot.title.position = "plot",
    plot.margin = margin(5, 5, 10, -10),
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-05-23", "20230523.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
