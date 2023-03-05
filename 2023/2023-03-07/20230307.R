library(tidyverse)
library(lubridate)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)

# load data
numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

# data wrangling
calendar_data <- numbats |> 
  select(eventDate) |> 
  drop_na(eventDate) |> 
  mutate(eventDate = as_date(eventDate)) |> 
  filter(year(eventDate) >= 2016 & year(eventDate) <= 2022) |> 
  group_by(eventDate) |> 
  summarise(n = n()) |> 
  arrange(desc(n)) |> 
  mutate(n = cut(n, breaks = c(0, 5, 10, 15)))

# make calendar
all_dates <- tibble(
  date = seq(ymd("20160101"), ymd("20221231"), by = "1 day")
)
calendar_df <- all_dates %>%
  mutate(Year = year(date),
         Month = month(date, label = TRUE),
         Day = wday(date, label = TRUE, week_start = 1),
         mday = mday(date),
         Month_week = (5 + day(date) + 
                         wday(floor_date(date, 'month'), week_start = 1)) %/% 7) |> 
  left_join(calendar_data, by = c("date" = "eventDate"))

# start recording
gg_record(
  dir = file.path("2023", "2023-03-07", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 12, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# text
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#519E8A;'>&#xf099;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>@nrennie35</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#519E8A;'>&#xf4f6;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>fosstodon.org/@nrennie</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#519E8A;'>&#xf09b;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>nrennie</span><span style='color:white;'>..</span>"

st <- glue("Numbats are small, distinctively-striped, insectivorous marsupials found 
           in Australia. The species was once widespread across southern Australia, 
           but is now restricted to several small colonies in Western Australia. They 
           are therefore considered an endangered species. The calendar below shows the
           number of sightings of numbats per day between  2016 and 2022, using data from 
           the Atlas of Living Australia. The full dataset includes data from 1856 to 2023 
           and, of the 805 observations, only 552 had dates recorded. Therefore the calendar may 
           not reflect all numbat sightings.")

# plot
ggplot(data = calendar_df,
       mapping = aes(x = Day, y = Month_week)) +
  geom_tile(fill = "transparent", colour = "grey80") +
  geom_point(data = drop_na(calendar_df),
             mapping = aes(colour = n, fill = n),
             pch = 21,
             size = 2.4,
             alpha = 0.4) +
  geom_text(aes(label = mday)) +
  facet_grid(Year~Month, switch = "y") +
  scale_fill_manual(values = c("#D1495B", "#EDAE49", "#00798C"),
                    labels = c("Less than 5", "Between 5 and 10", "More than 10"),
                    name = "Number of numbat sightings: ") +
  scale_colour_manual(values = c("#D1495B", "#EDAE49", "#00798C"),
                      labels = c("Less than 5", "Between 5 and 10", "More than 10"),
                      name = "Number of numbat sightings: ") +
  labs(title = "Numbats",
       subtitle = st,
       caption = social) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.margin = margin(10, 10, 10, 10),
        text = element_text(
            family = "Commissioner",
            colour = "#546666",
            size = 28
          ),
        plot.title = element_text(
            family = "Fraunces",
            size = 60,
            hjust = 0,
            colour = "#2F4F4F"
          ),
        plot.subtitle = element_textbox_simple(
          lineheight = 0.4,
          family = "Commissioner",
          colour = "#546666",
          margin = margin(t = 5, b = 5)
        ),
        plot.caption = element_textbox_simple(
          lineheight = 0.4,
          hjust = 0,
          margin = margin(t = 10),
          family = "Commissioner",
          colour = "#546666"
        ),
        strip.text.y = element_text(angle = 90,
                                    margin = margin(r = 5)),
        legend.text = element_text(hjust = 0),
        legend.spacing.x = unit(0.1, "cm"),
        legend.position = "top",
        legend.justification='left')

# save gif
gg_playback(
  name = file.path("2023", "2023-03-07", "20230307.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  image_resize = 800,
  frame_duration = .25,
  background = "white"
)
