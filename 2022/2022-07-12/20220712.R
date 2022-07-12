library(tidyverse)
library(showtext)
library(lubridate)
library(rcartocolor)

# get data
flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

# add fonts
font_add_google(name = "Red Hat Display", family = "redhat")
font_add_google(name = "Cherry Cream Soda", family = "cherry")
showtext_auto()

# prep data
plot_data <- flights %>% 
  filter(str_detect(APT_NAME, "London ")) %>% 
  mutate(airport = str_extract(APT_NAME, "(?<=- ).*")) %>% 
  mutate(yr_week = str_c(isoyear(FLT_DATE),
                         "/",
                         formatC(isoweek(FLT_DATE), format = "f", digits = 0, width = 2, flag = "0"))) %>% 
  mutate(week_day = wday(FLT_DATE, week_start = 1)) %>% 
  select(airport, yr_week, week_day, FLT_TOT_1) %>% 
  group_by(airport, yr_week, week_day) %>% 
  summarise(n_flights = sum(FLT_TOT_1)) %>% 
  ungroup() %>% 
  complete(airport, yr_week, week_day) %>% 
  mutate(n_flights = replace_na(n_flights, 0))

# get factor levels for airports
fact_levs <- plot_data %>% 
  group_by(airport) %>% 
  summarise(n = sum(n_flights)) %>% 
  arrange(-n) %>% 
  pull(airport)

# set factors
plot_data <- plot_data %>% 
  mutate(airport = factor(airport, levels = fact_levs))

# subtitle
st = str_wrap("The number of flights leaving from or arriving at London airports is still only around half of the pre-pandemic levels. London City has been slower than other London airports to resume services.",
              100)

# plot heatmap
ggplot(data = plot_data, 
       mapping = aes(x = yr_week, y = week_day, fill = n_flights)) +
  geom_raster() +
  facet_wrap(~airport, ncol = 1, strip.position="left") +
  scale_y_reverse(breaks = 7:1,
                     labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
                      sec.axis = sec_axis(~.,
                                          breaks = 7:1,
                                          labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
                     ) +
  scale_x_discrete(breaks = c("2016/01", "2017/01", "2018/01", "2019/01", "2020/01", "2021/01", "2022/01"), 
                   labels = 2016:2022) +
  scale_fill_carto_c(name = "Number of daily flights", palette = "SunsetDark", direction = 1, limits = c(0, 1500)) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(x = "", 
       y = "",
       title = "London Airports",
       subtitle = st, 
       caption = "N. Rennie | Data: Eurocontrol | #TidyTuesday") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
        plot.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
        strip.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
        legend.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
        legend.key.width = unit(2.5, "cm"),
        axis.ticks = element_blank(), 
        axis.text.y.left = element_blank(), 
        plot.margin = unit(c(0.5, 1, 0.5, 0.5), unit = "cm"), 
        plot.title = element_text(family = "cherry", hjust = 0.5, size = 20, face = "bold", 
                                  margin = margin(t = 10, b = 10)),
        strip.text = element_text(family = "cherry", hjust = 0.5, size = 10), 
        plot.subtitle = element_text(family = "redhat", hjust = 0.5, size = 12, 
                                     margin = margin(b = 10)), 
        plot.caption = element_text(family = "redhat", hjust = 0.5, size = 10), 
        axis.text.y.right = element_text(family = "redhat", hjust = 0.5, size = 9), 
        axis.text.x = element_text(family = "redhat", hjust = 0.5, size = 12), 
        legend.title = element_text(family = "redhat", hjust = 0.5, size = 12),
        legend.text = element_text(family = "redhat", hjust = 0.5, size = 10))

