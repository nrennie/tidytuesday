library(tidyverse)
library(showtext)
library(camcorder)

# load fonts
font_add_google("Chewy", "chewy")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()

# read in data
colors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
elements <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/elements.csv.gz')
parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/parts.csv.gz')
inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')

# prep data

color_elements <- left_join(elements, colors, by = c("color_id" = "id"))
color_elements <- color_elements %>%
  select(part_num, rgb)

plot_df <- color_elements %>%
  group_by(rgb) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  filter(n >= 100) %>%
  mutate(rgb = paste0("#", rgb)) %>%
  mutate(perc_tot = cumsum(n)/sum(n)) %>%
  mutate(row_n = row_number()) %>%
  mutate(row_n = if_else(row_n < 10, paste0("0", row_n), as.character(row_n))) %>%
  arrange(desc(n))

# start recording
gg_record(
  dir = file.path("2022", "2022-09-06", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 3, # width of saved image
  height = 8, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# plot
ggplot(data = plot_df) +
  geom_rect(aes(xmin = 0, xmax = 10000, ymin = factor(59), ymax = Inf),
            colour = "transparent",
            fill = "#CACACA") +
  geom_rect(aes(xmin = 0, xmax = 10000, ymin = -Inf, ymax = factor(59)),
            colour = "transparent",
            fill = "#e1e1e1") +
  geom_col(aes(y = row_n,
               x = n,
               fill = I(rgb))) +
  annotate("text",
           x = 5500,
           y = 26,
           label = "Lego Colors",
           hjust = 0.5,
           size = 24,
           family = "chewy") +
  annotate("text",
           x = 5500,
           y = 15,
           label = usefunc::str_wrap_break("50% of Lego parts are made of just 9 colors.\n\n*only colors with at least 100 parts shown here.", 24),
           hjust = 0.5,
           size = 12,
           lineheight = 0.4,
           family = "ubuntu") +
  annotate("text",
           x = 5500,
           y = 5,
           label = "N. Rennie | Data: {rebrickable}",
           hjust = 0.5,
           size = 10,
           family = "ubuntu") +
  annotate("text",
           x = 8500,
           y = 63,
           label = "50%",
           hjust = 0.5,
           colour = "black",
           size = 20,
           family = "chewy") +
  annotate("text",
           x = 8500,
           y = 55,
           label = "50%",
           hjust = 0.5,
           colour = "black",
           size = 20,
           family = "chewy") +
  labs(x = "Number of parts") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 10),
                                    size = 24,
                                   family = "ubuntu"),
        axis.title.x = element_text(margin = margin(t = 10),
                                    size = 24,
                                    family = "ubuntu"),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), unit = "cm"))

gg_playback(
  name = file.path("2022", "2022-09-06","20220906.gif"),
  first_image_duration = 8,
  last_image_duration = 12,
  frame_duration = .25
)
