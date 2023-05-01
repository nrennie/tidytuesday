library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(ggragged)
library(nrBrand)

# load fonts
font_add_google("Special Elite", "elite")
showtext_auto()

# read in data
plots <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/plots.csv')
species <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')
surveys <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/surveys.csv')

# prep data
plot_data <- surveys |> 
  select(year, plot, species) |>
  filter(plot %in% plots$plot) |> 
  drop_na() |> 
  group_by(year, plot, species) |> 
  summarise(n = n()) |> 
  left_join(select(species, species, commonname), by = "species") |> 
  filter(str_detect(commonname, "rat")) |>
  left_join(plots) |> 
  mutate(plot = factor(paste0("Plot ", plot),
                       levels = c("Plot 3", "Plot 4", "Plot 11", "Plot 14",
                                  "Plot 15", "Plot 17","Plot 19", "Plot 21")))

# start recording
gg_record(
  dir = file.path("2023", "2023-05-02", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 10, # width of saved image
  height = 10, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "grey95"
bg_col2 <- "grey75"
highlight_col <- "#B23A48"
control_col <- "#1F7A8C"
dark_col <- "grey15"

# text
title <- "The Portal Project"
st <- glue("The Portal Project is a long-term ecological research site studying
the dynamics of desert rodents, plants, ants and weather in Arizona. This chart
shows the number of rats in 8 different plots. Plots 3, 15, 19, and 21 are
<span style='color:{highlight_col};'>exclosure&nbsp; plots</span> whilst the rest 
are <span style='color:{control_col};'>control&nbsp;plots</span>. Merriam's 
kangaroo rat has been surveyed the most often, especially in control plots.")
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = dark_col,
  font_family = "elite"
)

# plot
ggplot(data = plot_data,
       mapping = aes(x = year, y = n, fill = treatment)) +
  geom_area() +
  facet_ragged_rows(
    vars(plot),
    vars(commonname)) +
  labs(title = title,
       subtitle = st,
       caption = social,
       x = "",
       y = "") +
  scale_fill_manual(values = c(control_col, highlight_col)) +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  scale_y_continuous(breaks = c(0, 75, 150),
                     limits = c(0, 150)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 28, base_family = "Commissioner") +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(
          fill = bg_col,
          colour = bg_col),
        panel.background = element_rect(
          fill = bg_col2,
          colour = bg_col2),
        plot.title = element_text(
          family = "elite",
          size = 54),
        plot.subtitle = element_textbox_simple(
          lineheight = 0.55,
          colour = dark_col,
          width = 0.9,
          family = "Commissioner",
          hjust = 0,
          margin = margin(b = 10),
          halign = 0
        ),
        plot.caption = element_textbox_simple(
          lineheight = 0.55,
          colour = dark_col,
          family = "Commissioner",
          hjust = 1.13,
          halign = 1.13,
          size = 30,
          margin = margin(r = -5),
        ),
        strip.text.y = element_text(
          face = "bold",
                                    size = 30,
                                    family = "elite",
                                    angle = 0),
        strip.text.x = element_text(
          margin = margin(0.1,0,0.1,0, "cm")
          ),
        legend.position = "none",
        panel.spacing = unit(0.6, "lines"),
        plot.margin = margin(15, 5, 10, 0))

# save gif
gg_playback(
  name = file.path("2023", "2023-05-02", "20230502.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
