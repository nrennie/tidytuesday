library(tidyverse)
library(showtext)
library(camcorder)
library(gganimate)

# load fonts
font_add_google("Atkinson Hyperlegible", "legible")
showtext_auto()

# load data
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

# data wrangling
plot_data <- artists |> 
  select(year, artist_race_nwi, space_ratio_per_page_total) |> 
  group_by(artist_race_nwi) |> 
  summarise(space = mean(space_ratio_per_page_total))

# start recording
gg_record(
  dir = file.path("2023", "2023-01-17", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# subtitle
st <- "Each white box represents a page of either Janson's History of Art and Gardner's Art Through the Ages, and the blue box indicates the average page space that both the text and the figure of artists are given. On average, white artists are allocated 54% of the page, whilst non-white artists are given only 40%."

# plot
ggplot() +
  geom_rect(data = data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
            mapping = aes(xmin = xmin, ymin = ymin,
                          xmax = xmax, ymax = ymax),
            fill = "white",
            colour = "#404040",
            linewidth = 0.3) +
  geom_rect(data = plot_data,
            mapping = aes(xmin = 0.5-sqrt(space)/2,
                          xmax = 0.5+sqrt(space)/2,
                          ymin = 0.5-sqrt(space)/2,
                          ymax = 0.5+sqrt(space)/2),
            fill = "#8499B1") +
  geom_text(data = plot_data,
            mapping = aes(x = 0.5, y = 0.5, label = paste0(round(space*100), "%")),
            colour = "white",
            family = "legible",
            size = 14,
            fontface = "bold") +
  facet_wrap(~artist_race_nwi) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE) +
  labs(title = "TAKING UP SPACE IN ART HISTORY",
       subtitle = str_wrap(st, 85),
       caption = "N. Rennie | Data: {arthistory}") +
  theme(plot.background = element_rect(fill = "#dedede", colour = "#dedede"),
        panel.background = element_rect(fill = "#dedede", colour = "#dedede"),
        strip.background = element_rect(fill = "#dedede", colour = "#dedede"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(size = 32,
                                  face = "bold",
                                  family = "legible"),
        plot.title = element_text(size = 40,
                                  face = "bold",
                                  family = "legible",
                                  margin = margin(t = 10, b = 10)),
        plot.subtitle = element_text(size = 32,
                                     family = "legible",
                                     lineheight = 0.4),
        plot.caption = element_text(size = 32,
                                    family = "legible",
                                    hjust = 0,
                                    margin = margin(t = 10)),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title.position = "plot",
        aspect.ratio = 1.414,
        plot.margin = margin(10,10,10,10))
  
# save gif
gg_playback(
  name = file.path("2023", "2023-01-17","20230117.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)


# By book -----------------------------------------------------------------

# data wrangling
plot_data <- artists |> 
  select(year, book, artist_race_nwi, space_ratio_per_page_total) |> 
  group_by(artist_race_nwi, book) |> 
  summarise(space = mean(space_ratio_per_page_total))
# plot
ggplot() +
  geom_rect(data = data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
            mapping = aes(xmin = xmin, ymin = ymin,
                          xmax = xmax, ymax = ymax),
            fill = "white",
            colour = "#404040",
            linewidth = 0.3) +
  geom_rect(data = plot_data,
            mapping = aes(xmin = 0.5-sqrt(space)/2,
                          xmax = 0.5+sqrt(space)/2,
                          ymin = 0.5-sqrt(space)/2,
                          ymax = 0.5+sqrt(space)/2),
            fill = "#8499B1") +
  geom_text(data = plot_data,
            mapping = aes(x = 0.5, y = 0.5, label = paste0(round(space*100), "%")),
            colour = "white",
            family = "legible",
            size = 12,
            fontface = "bold") +
  facet_grid(book~artist_race_nwi) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE) +
  labs(title = "TAKING UP SPACE IN ART HISTORY",
       subtitle = str_wrap("Janson's History of Art gives non-white artists an even smaller area on the page compared to Gardner's Art Through the Ages", 55),
       caption = "N. Rennie | Data: {arthistory}") +
  theme(plot.background = element_rect(fill = "#dedede", colour = "#dedede"),
        panel.background = element_rect(fill = "#dedede", colour = "#dedede"),
        strip.background = element_rect(fill = "#dedede", colour = "#dedede"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(size = 32,
                                  face = "bold",
                                  family = "legible"),
        plot.title = element_text(size = 40,
                                  face = "bold",
                                  family = "legible",
                                  margin = margin(t = 10, b = 10)),
        plot.subtitle = element_text(size = 32,
                                     family = "legible",
                                     lineheight = 0.4),
        plot.caption = element_text(size = 32,
                                    family = "legible",
                                    hjust = 0,
                                    margin = margin(t = 10)),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title.position = "plot",
        aspect.ratio = 1.414,
        plot.margin = margin(10,10,10,10))



# By gender ---------------------------------------------------------------

# data wrangling
plot_data <- artists |> 
  select(year, artist_gender, artist_race_nwi, space_ratio_per_page_total) |> 
  group_by(artist_race_nwi, artist_gender) |> 
  summarise(space = mean(space_ratio_per_page_total))
# plot
ggplot() +
  geom_rect(data = data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
            mapping = aes(xmin = xmin, ymin = ymin,
                          xmax = xmax, ymax = ymax),
            fill = "white",
            colour = "#404040",
            linewidth = 0.3) +
  geom_rect(data = plot_data,
            mapping = aes(xmin = 0.5-sqrt(space)/2,
                          xmax = 0.5+sqrt(space)/2,
                          ymin = 0.5-sqrt(space)/2,
                          ymax = 0.5+sqrt(space)/2),
            fill = "#8499B1") +
  geom_text(data = plot_data,
            mapping = aes(x = 0.5, y = 0.5, label = paste0(round(space*100), "%")),
            colour = "white",
            family = "legible",
            size = 12,
            fontface = "bold") +
  facet_grid(artist_gender~artist_race_nwi) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE) +
  labs(title = "TAKING UP SPACE IN ART HISTORY",
       caption = "N. Rennie | Data: {arthistory}") +
  theme(plot.background = element_rect(fill = "#dedede", colour = "#dedede"),
        panel.background = element_rect(fill = "#dedede", colour = "#dedede"),
        strip.background = element_rect(fill = "#dedede", colour = "#dedede"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(size = 32,
                                  face = "bold",
                                  family = "legible"),
        plot.title = element_text(size = 40,
                                  face = "bold",
                                  family = "legible",
                                  margin = margin(t = 10, b = 10)),
        plot.subtitle = element_text(size = 32,
                                     family = "legible",
                                     lineheight = 0.4),
        plot.caption = element_text(size = 32,
                                    family = "legible",
                                    hjust = 0,
                                    margin = margin(t = 10)),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title.position = "plot",
        aspect.ratio = 1.414,
        plot.margin = margin(10,10,10,10))



# By year (animated) ------------------------------------------------------

# data wrangling
plot_data <- artists |> 
  select(year, year, artist_race_nwi, space_ratio_per_page_total) |> 
  group_by(artist_race_nwi, year) |> 
  summarise(space = mean(space_ratio_per_page_total)) |> 
  ungroup() |> 
  mutate(year = factor(year)) |> 
  mutate(label = paste0(round(space*100), "%"))
# plot
g <- ggplot() +
  geom_rect(data = data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
            mapping = aes(xmin = xmin, ymin = ymin,
                          xmax = xmax, ymax = ymax),
            fill = "white",
            colour = "#404040",
            linewidth = 0.3) +
  geom_rect(data = plot_data,
            mapping = aes(xmin = 0.5-sqrt(space)/2,
                          xmax = 0.5+sqrt(space)/2,
                          ymin = 0.5-sqrt(space)/2,
                          ymax = 0.5+sqrt(space)/2),
            fill = "#8499B1") +
  geom_text(data = plot_data,
            mapping = aes(x = 0.5, y = 0.5, label = label),
            colour = "white",
            family = "legible",
            size = 12,
            fontface = "bold") +
  facet_wrap(~artist_race_nwi) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE) +
  labs(title = "TAKING UP SPACE IN {current_frame}",
       caption = "N. Rennie | Data: {arthistory}") +
  theme(plot.background = element_rect(fill = "#dedede", colour = "#dedede"),
        panel.background = element_rect(fill = "#dedede", colour = "#dedede"),
        strip.background = element_rect(fill = "#dedede", colour = "#dedede"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(size = 10,
                                  face = "bold",
                                  family = "legible"),
        plot.title = element_text(size = 14,
                                  face = "bold",
                                  family = "legible",
                                  margin = margin(t = 10, b = 10)),
        plot.subtitle = element_text(size = 10,
                                     family = "legible",
                                     lineheight = 0.4),
        plot.caption = element_text(size = 10,
                                    family = "legible",
                                    hjust = 0,
                                    margin = margin(t = 10)),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title.position = "plot",
        aspect.ratio = 1.414,
        plot.margin = margin(10,10,10,10)) 

g + 
  transition_manual(year)
anim_save("2023/2023-01-17/20230117_year.gif")


# By year (static) --------------------------------------------------------

# data wrangling
plot_data <- artists |> 
  select(year, year, artist_race_nwi, space_ratio_per_page_total) |> 
  group_by(artist_race_nwi, year) |> 
  summarise(space = mean(space_ratio_per_page_total)) |> 
  ungroup() |> 
  mutate(label = paste0(round(space*100), "%")) |> 
  drop_na()
# plot
ggplot(data = plot_data,
       mapping = aes(x = year,
                     y = space,
                     colour = artist_race_nwi)) +
  geom_line() +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_continuous(limits = c(1925, 2025)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "TAKING UP SPACE IN ART HISTORY",
       y = "Avg. percentage of page space",
       caption = "N. Rennie | Data: {arthistory}") +
  coord_cartesian(expand = FALSE) +
  theme(plot.background = element_rect(fill = "#dedede", colour = "#dedede"),
        legend.background = element_rect(fill = "#dedede", colour = "#dedede"),
        panel.background = element_rect(fill = "#dedede", colour = "#dedede"),
        strip.background = element_rect(fill = "#dedede", colour = "#dedede"),
        legend.key = element_rect(fill = "#dedede", colour = "#dedede"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(size = 32,
                                  face = "bold",
                                  family = "legible"),
        plot.title = element_text(size = 40,
                                  face = "bold",
                                  family = "legible",
                                  margin = margin(t = 10, b = 10)),
        plot.subtitle = element_text(size = 32,
                                     family = "legible",
                                     lineheight = 0.4),
        plot.caption = element_text(size = 32,
                                    family = "legible",
                                    hjust = 0,
                                    margin = margin(t = 10)),
        axis.text = element_text(size = 32,
                                 family = "legible"),
        legend.text = element_text(size = 32,
                                   family = "legible",
                                   lineheight = 0.4),
        axis.title.y = element_text(size = 32,
                                    family = "legible",
                                    lineheight = 0.4),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        plot.title.position = "plot",
        legend.position = "top",
        legend.title = element_blank(),
        plot.margin = margin(10,20,10,10))
