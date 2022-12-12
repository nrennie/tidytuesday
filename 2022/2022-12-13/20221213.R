library(tidyverse)
library(showtext)
library(camcorder)
library(patchwork)
library(lubridate)

# load fonts
font_add_google("Raleway", "raleway")
font_add_google("Passion One", "passion")
showtext_auto()

# load data
state_retail <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/state_retail.csv',  col_types = "cciciiccc")
coverage_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/coverage_codes.csv')

# data wrangling
plot_data <- state_retail %>% 
  filter(state_abbr == "NY") %>% 
  filter(subsector %in% c("Food and Beverage", "Gasoline Stations", "Health and Personal Care")) %>% 
  mutate(change_yoy = as.numeric(change_yoy),
         change_yoy_se = as.numeric(change_yoy_se)) %>% 
  mutate(date = paste0("01/", month, "/", year)) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(lower1 = change_yoy - change_yoy_se,
         upper1 = change_yoy + change_yoy_se) %>% 
  mutate(lower2 = change_yoy - 2*change_yoy_se,
         upper2 = change_yoy + 2*change_yoy_se) %>% 
  mutate(lower3 = change_yoy - 3*change_yoy_se,
         upper3 = change_yoy + 3*change_yoy_se) %>% 
  select(c(subsector, date, change_yoy, lower1, upper1, lower2, upper2, lower3, upper3)) %>% 
  drop_na()

# start recording
gg_record(
  dir = file.path("2022", "2022-12-13", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# subtitle
st <- usefunc::str_wrap_break("The Monthly State Retail Sales (MSRS) is the Census Bureau's new experimental data product featuring modeled state-level retail sales. This graphic shows the year-on-year percentage change in retail sales value for three categories of product in the state of New York. The shaded areas represent one, two, and three standard deviations of the estimated change.\n\nN. Rennie | Data: US Census Bureau", 115)

# plot
p1 <- ggplot(data = plot_data) +
  geom_hline(yintercept = 0, colour = "#BB0A21") +
  geom_ribbon(mapping = aes(x = date,
                            ymin = lower3,
                            ymax = upper3,
                            fill = subsector),
              fill = "#374A67",
              alpha = 0.2) +
  geom_ribbon(mapping = aes(x = date,
                            ymin = lower2,
                            ymax = upper2),
              fill = "#374A67",
              alpha = 0.2) +
  geom_ribbon(mapping = aes(x = date,
                            ymin = lower1,
                            ymax = upper1),
              fill = "#374A67",
              alpha = 0.2) +
  geom_line(mapping = aes(x = date, 
                          y = change_yoy),
            colour = "#374A67") +
  facet_wrap(~subsector, ncol = 3) +
  labs(x = "",
       y = str_wrap("Year-on-year % change in retail sales value", 27),
       title = "New York Retail Sales",
       subtitle = st) +
  coord_cartesian(expand = FALSE) +
  theme(plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.grid.major = element_line(colour = "#c8c8c8"),
        axis.ticks = element_blank(),
        axis.title.y = element_text(family = "raleway", lineheight = 0.4, size = 20),
        axis.text = element_text(family = "raleway", lineheight = 0.4, size = 20),
        plot.title = element_text(family = "passion", size = 80),
        plot.subtitle = element_text(margin = margin(b = 20), family = "raleway", lineheight = 0.4, size = 24),
        plot.margin = margin(10, 15, 0, 10),
        strip.background = element_blank(),
        strip.text.x = element_text(family = "raleway", lineheight = 0.4, size = 20, face = "bold"),
        plot.title.position = "plot")
p1

p2 <- ggplot(plot_data) +
  geom_tile(mapping = aes(x = month(date, label = TRUE),
                          y = year(date),
                          fill = change_yoy),
            linewidth = 2,
            colour = "#fafafa") +
  facet_wrap(~subsector, ncol = 3) +
  scale_x_discrete(labels = function(x) str_sub(x, 1, 1)) +
  scale_y_reverse() +
  scale_fill_gradient2(low = "#BB0A21", high = "#374A67") +
  labs(x = "", y = "") +
  coord_fixed() +
  theme(plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(family = "raleway", lineheight = 0.4, size = 24),
        axis.text = element_text(family = "raleway", lineheight = 0.4, size = 20),
        plot.title = element_text(family = "passion", size = 80),
        plot.subtitle = element_text(margin = margin(b = 20), family = "raleway", lineheight = 0.4, size = 24),
        plot.margin = margin(0, 15, 10, 10),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.title.position = "plot")
p2

# combine plots
p1 + p2 + plot_layout(nrow = 2) &
  theme(plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"))
fname <- paste0(format(Sys.time(), "%Y_%m_%d_%H_%M_%OS6"), ".png")
ggsave(filename = fname, height = 4, width = 6, unit = "in", dpi = 300)

# save gif
gg_playback(
  name = file.path("2022", "2022-12-13","20221213.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25
)
