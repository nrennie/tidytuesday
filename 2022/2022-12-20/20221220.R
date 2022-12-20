library(tidyverse)
library(camcorder)
library(showtext)
library(lubridate)
library(PrettyCols)
library(geofacet)
library(usefunc)
library(patchwork)

# load fonts
font_add_google("Roboto", "roboto")
font_add_google("Carter One", "carter")
showtext_auto()

# get the data
weather_forecasts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/weather_forecasts.csv')
cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/cities.csv')
outlook_meanings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/outlook_meanings.csv')

# prep data
plot_data <- weather_forecasts %>% 
  filter(forecast_hours_before == 12,
         high_or_low == "high") %>% 
  mutate(date = floor_date(date, "month")) %>% 
  select(c(date, state, observed_temp, forecast_temp)) %>% 
  drop_na() %>% 
  mutate(error = case_when(observed_temp < forecast_temp ~ "Under forecast",
                           observed_temp == forecast_temp ~ "Correct forecast",
                           observed_temp > forecast_temp ~ "Over forecast")) %>% 
  group_by(date, state) %>% 
  mutate(tot_n = n()) %>% 
  ungroup() %>% 
  group_by(date, state, error) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n/tot_n) %>% 
  select(date, state, error, prop) %>% 
  distinct() %>% 
  mutate(error = factor(error, levels = c("Over forecast", "Correct forecast", "Under forecast"))) %>% 
  filter(state %notin% c("VI", "PR"))

# start recording
gg_record(
  dir = file.path("2022", "2022-12-20", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# legend plot
p_data <- filter(plot_data, state == "TX")
text_labels <- tibble(date = rep(max(p_data$date)+5, 3),
                      prop = c(0.2, 0.5, 0.8),
                      label = str_wrap(c("Forecast below observed value",
                                "Forecast equal to observed value",
                                "Forecast above observed value"), 20),
                      error = factor(c("Under forecast", "Correct forecast", "Over forecast"),
                                     levels = c("Over forecast", "Correct forecast", "Under forecast")))

p_leg <- ggplot() +
  geom_area(data = p_data,
            mapping = aes(x = date, y = prop, fill = error),
            position = "fill") +
  geom_text(data = text_labels,
            mapping = aes(x = date, y = prop, label = label, colour = error),
            hjust = 0,
            lineheight = 0.4,
            size = 6,
            family = "roboto") +
  geom_text(data = data.frame(x = min(p_data$date)+50,
                              y = 0.8,
                              label = "TX"),
            mapping = aes(x = x, y = y, label = label),
            family = "carter",
            colour = alpha("#fafafa", 0.7),
            size = 10) +
  coord_cartesian(expand = FALSE) +
  scale_x_date(limits = c(min(p_data$date), max(p_data$date)+150),
               breaks = ymd("2021-04-01", "2021-08-01", "2021-12-01", "2022-04-01"),
               labels = c("Apr 2021", "Aug 2021", "Dec 2021", "Apr 2022")) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = rev(PrettyCols::prettycols("Beach")[c(2, 4, 5)])) +
  scale_colour_manual(values = rev(PrettyCols::prettycols("Beach")[c(2, 4, 5)])) +
  labs(x = "",
       y = "Proportion of forecasts") +
  theme_minimal(base_family = "roboto", base_size = 20) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(margin = margin(r = 5), family = "roboto"))

# subtitle
st <- str_wrap_break("Forecasts for higher temperatures tend to overestimate how hot it's going to be. Across 81,496 forecasts for high temperatures estimated 12 hours previously, 34,629 over-estimated the temperature. This compares to 24,995 cases of under-estimating the temperature.\n\nN. Rennie | Data: USA National Weather Service", 55)

# main plot
state_labels = tibble(state = us_state_grid2$code,
                      y = 0.5,
                      date = mean(c(min(p_data$date), max(p_data$date))))

p_main <- ggplot(data = plot_data) +
  geom_area(mapping = aes(x = date, y = prop, fill = error, group = error),
            position = "fill") +
  geom_text(data = state_labels,
            mapping = aes(x = date, 
                          y = y, 
                          label = state),
            family = "carter",
            colour = alpha("#fafafa", 0.7),
            size = 10) +
  facet_geo(~state, grid = "us_state_grid2") +
  coord_cartesian(expand = FALSE) +
  scale_x_date(limits = c(min(p_data$date), max(p_data$date))) +
  scale_y_continuous(breaks = c(0, 1)) +
  scale_fill_manual(values = rev(PrettyCols::prettycols("Beach")[c(2, 4, 5)])) +
  scale_colour_manual(values = rev(PrettyCols::prettycols("Beach")[c(2, 4, 5)])) +
  labs(x = "",
       y = "",
       title = "Higher or lower?",
       subtitle = st) +
  theme_minimal(base_family = "roboto", base_size = 20) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "carter", size = 60, lineheight = 0.1, margin = margin(b = 0)),
        plot.subtitle = element_text(family = "roboto", lineheight = 0.4, margin = margin(t = -10, b = 5)),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = margin(10,5,5,5))
record_polaroid()

# combine plots
p_main + inset_element(p_leg, 0.48, 0.6, 0.99, 0.9, align_to = "full", clip = FALSE) &
  theme(plot.margin = margin(0, 5, -10, -10),
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"))
record_polaroid()

# save gif
gg_playback(
  name = file.path("2022", "2022-12-20","20221220.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25
)
