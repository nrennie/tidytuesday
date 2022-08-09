library(tidyverse)
library(showtext)
library(cowplot)
library(usefunc)
library(rcartocolor)
library(emojifont)
library(purrr)

# font
font_add_google(name = "Gravitas One", family = "gravitas")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext::showtext_auto()

# load data
wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

# prep data
wheels_data <- wheels %>% 
  filter(!is.na(diameter), !is.na(number_of_cabins)) %>% 
  filter(country == "USA") %>% 
  select(name, diameter, number_of_cabins)  

wheels_time <- usefunc::rep_df(wheels_data, times = 30) %>% 
  mutate(time = rep(1:30, each = nrow(wheels_data)))

plot_data <- wheels_time %>% 
  mutate(starting_angles = rep(seq(0, by = 360/nrow(wheels_data), length.out = nrow(wheels_data)), times = 30), 
         x = 0.5*diameter*cos(pi*(starting_angles-time*12)/180), 
         y = 0.5*diameter*sin(pi*(starting_angles-time*12)/180))

# subtitle
st <- usefunc::str_wrap_break("The {ferriswheels} package contains data on 73 ferris wheel, 19 of which are located in the USA. Here, only ferris wheels with known diameters are represented. The length of the lines represents the diameter of the ferris wheels, and the size represents the number of cabins (between 24 and 40).  \nN. Rennie | Data: {ferriswheels}", 80)

# plot function
# gganimate had issues with emojifont so purrr and manual gif instead
plot_time <- function(t) {
  p <- ggplot(filter(plot_data, time == t), aes(x = x, y = y)) +
    geom_segment(aes(xend = 0, yend = 0, group = name)) +
    geom_point(aes(colour = name), size = 0) +
    geom_text(aes(label = emoji("ferris_wheel"),
                  colour = name, 
                  size = number_of_cabins),
              family='EmojiOne') +
    xlim(-400, 400) +
    ylim(-400, 400) +
    labs(title = "Ferris Wheels", 
         subtitle = st) +
    coord_fixed() + 
    scale_size(range = c(3, 8), guide = 'none') +
    scale_colour_carto_d(name = "", palette = "Bold") +
    guides(colour = guide_legend(ncol = 3, override.aes = list(size = 7))) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "gravitas"), 
          plot.subtitle = element_text(hjust = 0.5, size = 10,
                                       margin = margin(t = 10, b = 20), family = "ubuntu"),
          legend.title = element_text(hjust = 0.5, size = 10, family = "ubuntu"),
          legend.text = element_text(size = 8, family = "ubuntu", margin = margin(r = 20)),
          legend.position = "top", 
          plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
          panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"))
  png(paste0("2022/2022-08-09/anim/plot", t, ".png"), width = 700, height = 672)
  print(cowplot::ggdraw(p) + 
    theme(plot.background = element_rect(fill="#fafafa", color = NA)))
  dev.off()
}

# purrr
purrr::map(.x = 1:30, .f=~plot_time(.x))

# static plot (all times)
p <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_segment(aes(xend = 0, yend = 0, group = name)) +
  geom_point(aes(colour = name), size = 0) +
  geom_text(aes(label = emoji("ferris_wheel"),
                colour = name, 
                size = number_of_cabins),
            family='EmojiOne') +
  xlim(-400, 400) +
  ylim(-400, 400) +
  labs(title = "Ferris Wheels", 
       subtitle = st) +
  coord_fixed() + 
  scale_size(range = c(3, 8), guide = 'none') +
  scale_colour_carto_d(name = "", palette = "Bold") +
  guides(colour = guide_legend(override.aes = list(size = 7))) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "gravitas"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10,
                                     margin = margin(t = 10, b = 20), family = "ubuntu"),
        legend.title = element_text(hjust = 0.5, size = 10, family = "ubuntu"),
        legend.text = element_text(size = 10, family = "ubuntu"),
        legend.position = "top", 
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"))
cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="#fafafa", color = NA))
