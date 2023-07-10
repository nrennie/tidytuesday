library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)

# load data
global_temps <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv")
nh_temps <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv")
sh_temps <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv")

# prep data
plot_data <- global_temps |>
  select(c(Year, Jan:Dec)) |>
  pivot_longer(-Year, names_to = "Month", values_to = "Temp") |>
  drop_na() |> 
  mutate(Month = factor(Month,
                        levels = month.abb)) |> 
  mutate(t = row_number())

# smooth join between Dec and Jan
padding <- plot_data[plot_data$Month == 'Jan',]
padding$Year <- padding$Year - 1 
padding$Month <- NA

# start recording
gg_record(
  dir = file.path("2023", "2023-07-11", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 4, # width of saved image
  height = 6.5, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "grey30"
highlight_col <- "#d6604d"
text_col <- "white"

# text
title <- "Global Surface Temperatures"
st <- "The GISS Surface Temperature Analysis version 4 (GISTEMP v4) 
is an estimate of global surface temperature change, combining land-surface,
air and sea-surface water temperature anomalies. Here, values relate to 
deviations from the 1951-1980 average."
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#d6604d;'>&#xf099;</span><span style='color:grey30;'>.</span><span style='font-family:Commissioner;color:white;'>@nrennie35</span><span style='color:grey30;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#d6604d;'>&#xf4f6;</span><span style='color:grey30;'>.</span><span style='font-family:Commissioner;color:white;'>fosstodon.org/@nrennie</span><span style='color:grey30;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#d6604d;'>&#xf09b;</span><span style='color:grey30;'>.</span><span style='font-family:Commissioner;color:white;'>nrennie</span><span style='color:grey30;'>..</span>"
cap <- paste0("**Data**: NASA GISS Surface Temperature Analysis<br>**Chart**: ",
              social)

# plot
ggplot() +
  geom_line(
    data = rbind(plot_data, padding),
    mapping = aes(x = Month, y = Temp, group = Year, colour = Temp)
  ) +
  labs(x = "", y = NULL,
       title = title,
       subtitle = st,
       caption = cap,
       tag = tail(plot_data, 1)$Year) +
  scale_colour_distiller(limits = c(-1.5, 1.5), palette = "RdBu") +
  scale_x_discrete(expand = c(0,0), breaks = month.abb) +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  guides(colour = guide_colorbar(title = "Temperature deviation (°C)",
                               title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 15,
                               barheight = 0.7)) +
  coord_polar() +
  theme_minimal(base_size = 24, base_family = "Commissioner") +
  theme(legend.position = c(0.5, -0.1),
        legend.direction = "horizontal",
        legend.text = element_text(colour = text_col),
        legend.title = element_text(colour = text_col),
        plot.tag.position = c(0.46, 0.75),
        plot.tag = element_textbox_simple(
          family = "Commissioner",
          colour = text_col,
          size = 36,
          hjust = 0,
          lineheight = 0.5,
          margin = margin(t = 20, b = 10)
        ),
        axis.text.x = element_text(colour = text_col),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3, 
                                          colour = alpha(text_col, 0.4)),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col),
        plot.margin = margin(10, 15, 10, 10),
        plot.title = element_textbox_simple(
          family = "Fraunces",
          colour = text_col,
          hjust = 0,
          size = 40,
          margin = margin(t = 20)
        ),
        plot.subtitle = element_textbox_simple(
          family = "Commissioner",
          colour = text_col,
          lineheight = 0.5,
          margin = margin(t = 20, b = 30)
        ),
        plot.caption = element_textbox_simple(
          family = "Commissioner",
          colour = text_col,
          hjust =  0,
          lineheight = 0.5,
          margin = margin(t = 40)
        )) 

# save gif
gg_playback(
  name = file.path("2023", "2023-07-11", "20230711.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

# new recording
gg_record(
  dir = file.path("2023", "2023-07-11", "gif"), # where to save the recording
  device = "png", # device to use to save images
  width = 4, # width of saved image
  height = 6.5, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# animate
for (i in 1:nrow(plot_data)) {
  p_data <- filter(plot_data, t <= i)
  p_padding <- p_data[p_data$Month == 'Jan',]
  p_padding$Year <- p_padding$Year - 1 
  p_padding$Month <- NA
  
  g <- ggplot() +
    geom_line(
      data = rbind(p_data, p_padding),
      mapping = aes(x = Month, y = Temp, group = Year, colour = Temp)
    ) +
    labs(x = "", y = NULL,
         title = title,
         subtitle = st,
         caption = cap,
         tag = tail(p_data, 1)$Year) +
    scale_colour_distiller(limits = c(-1.5, 1.5), palette = "RdBu") +
    scale_x_discrete(expand = c(0,0), breaks = month.abb, drop = FALSE) +
    scale_y_continuous(limits = c(-1.5, 1.5)) +
    guides(colour = guide_colorbar(title = "Temperature deviation (°C)",
                                   title.position = "top",
                                   title.hjust = 0.5,
                                   barwidth = 15,
                                   barheight = 0.7)) +
    coord_polar() +
    theme_minimal(base_size = 24, base_family = "Commissioner") +
    theme(legend.position = c(0.5, -0.1),
          legend.direction = "horizontal",
          legend.text = element_text(colour = text_col),
          legend.title = element_text(colour = text_col),
          plot.tag.position = c(0.46, 0.75),
          plot.tag = element_textbox_simple(
            family = "Commissioner",
            colour = text_col,
            size = 36,
            hjust = 0,
            lineheight = 0.5,
            margin = margin(t = 20, b = 10)
          ),
          axis.text.x = element_text(colour = text_col),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_line(linewidth = 0.3, 
                                            colour = alpha(text_col, 0.4)),
          panel.grid.major.y = element_blank(),
          plot.background = element_rect(fill = bg_col, colour = bg_col),
          panel.background = element_rect(fill = bg_col, colour = bg_col),
          plot.margin = margin(10, 15, 10, 10),
          plot.title = element_textbox_simple(
            family = "Fraunces",
            colour = text_col,
            hjust = 0,
            size = 40,
            margin = margin(t = 20)
          ),
          plot.subtitle = element_textbox_simple(
            family = "Commissioner",
            colour = text_col,
            lineheight = 0.5,
            margin = margin(t = 20, b = 30)
          ),
          plot.caption = element_textbox_simple(
            family = "Commissioner",
            colour = text_col,
            hjust =  0,
            lineheight = 0.5,
            margin = margin(t = 40)
          )) 
  print(g)
}

# save gif
gg_playback(
  name = file.path("2023", "2023-07-11", "20230711-animation.gif"),
  first_image_duration = 1,
  last_image_duration = 5,
  frame_duration = .1,
  background = bg_col,
  image_resize = 1950
)








