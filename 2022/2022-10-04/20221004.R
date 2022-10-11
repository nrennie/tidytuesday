library(tidyverse)
library(camcorder)
library(showtext)
library(lubridate)
library(ggbeeswarm)
library(rcartocolor)
library(usefunc)

# load fonts
font_add_google("Ubuntu Mono", "ubuntu")
showtext_auto()

# get the data
product_hunt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')

# data wrangling
plot_data <- product_hunt %>%
  filter(str_detect(category_tags, "'OPEN SOURCE'", negate = FALSE)) %>%
  select(name, release_date, upvotes) %>%
  arrange(release_date) %>%
  mutate(n = row_number(),
         cat = "A")

# start recording
gg_record(
  dir = file.path("2022", "2022-10-04", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 4, # width of saved image
  height = 5, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# subtitle
st <- str_wrap_break("Between 2014 and 2022, the number of open source products listed on Product Hunt increased, especially since 2016. Products that have been listed for longer, tend to have a higher number of upvotes, though not markedly so.", 70)

# plot
ggplot() +
  geom_beeswarm(data = plot_data,
                mapping = aes(x = cat,
                              y = release_date,
                              colour = upvotes),
                size = 0.5, priority = 'density') +
  geom_segment(data = slice_max(plot_data, upvotes, n = 1),
               mapping = aes(x = 1,
                             xend = 1,
                             y = min(plot_data$release_date),
                             yend = max(plot_data$release_date)),
               size = 0.3,
               alpha = 0.5) +
  geom_text(data = slice_max(plot_data, upvotes, n = 1),
            mapping = aes(x = 0.75,
                          y = release_date,
                          label = str_wrap_break("Released on 1 June 2017, Feather Icons is the most upvoted open source product with 5,887 upvotes.", 40)),
            size = 6,
            family = "ubuntu",
            lineheight = 0.4) +
  geom_segment(data = slice_max(plot_data, upvotes, n = 1),
               mapping = aes(x = 0.8,
                             xend = 0.95,
                             y = release_date,
                             yend = release_date),
               arrow = arrow(type = "closed",
                             length = unit(0.05, "inches")),
               size = 0.3) +
  coord_flip() +
  scale_color_carto_c(palette = "SunsetDark",
                      name = "Number of upvotes",
                      limits = c(0, 6000),
                      breaks = c(0, 3000, 6000)) +
  labs(x = "",
       y = "",
       title = "OPEN SOURCE PRODUCTS",
       tag = st,
       caption = "N. Rennie | Data: components.one | #TidyTuesday") +
  guides(colour = guide_colourbar(title.position = "top")) +
  theme(plot.title = element_text(family = "ubuntu",
                                  size = 40,
                                  hjust = 0.5),
        plot.tag = element_text(family = "ubuntu",
                                size = 24,
                                lineheight = 0.4),
        plot.tag.position = c(0.5, 0.8),
        plot.caption = element_text(family = "ubuntu",
                                    size = 20,
                                    hjust = 0.5),
        legend.text = element_text(family = "ubuntu",
                                   size = 18,
                                   hjust = 0),
        legend.title = element_text(family = "ubuntu",
                                    size = 18,
                                    hjust = 0.5),
        axis.text.x = element_text(family = "ubuntu",
                                 size = 18),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(family = "ubuntu",
                                  size = 20),
        axis.title.y = element_blank(),
        plot.margin = margin(c(0.5, 0.5, 0.5, 0.5), unit = "cm"),
        plot.background = element_rect(fill = "#fafafa",
                                       colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa",
                                        colour = "#fafafa"),
        legend.background = element_rect(fill = "transparent",
                                         colour = "transparent"),
        legend.position = c(0.5, 0.1),
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(1.7, "cm"),
        legend.direction = "horizontal",
        panel.grid = element_blank(),
        )

# save gif
gg_playback(
  name = file.path("2022", "2022-10-04","20221004.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)





