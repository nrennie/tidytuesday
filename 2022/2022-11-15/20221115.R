library(tidyverse)
library(lubridate)
library(camcorder)
library(changepoint)

# load data
image_alt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/image_alt.csv')
color_contrast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/color_contrast.csv')
ally_scores <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/ally_scores.csv')
bytes_total <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv')
speed_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/speed_index.csv')

# start recording
gg_record(
  dir = file.path("2022", "2022-11-15", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# Plot 1 ------------------------------------------------------------------

data1 <- bytes_total %>% 
  select(date, client, p50) %>% 
  mutate(date = ymd(date))

ggplot(data = data1,
       mapping = aes(x = date, y = p50, colour = client)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  annotate("text", x = ymd("2013-06-01"), y = 1300, label = "Desktop", colour = "#40b1d6", fontface = "bold") +
  annotate("text", x = ymd("2016-01-01"), y = 700, label = "Mobile", colour = "#23698c", fontface = "bold") +
  scale_colour_manual(values = c("#40b1d6", "#23698c")) +
  scale_x_date(breaks = seq(ymd("2011-01-01"), ymd("2022-01-01"), by = "1 year"),
               labels = 2011:2022,
               limits = ymd(c("2010-11-01", "2022-10-01"))) +
  scale_y_continuous(breaks = seq(0, 2200, 200),
                     labels = c(format(seq(0, 2000, 200), big.mark=","), "2,200 KB"),
                     minor_breaks = c(800, 1800),
                     limits = c(0, 2400), 
                     expand = c(0, 0)) +
  labs(title = "Average page weight", 
       caption = "Source: HTTPArchive | N. Rennie | #TidyTuesday") +
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgrey", linewidth = 0.3),
        panel.grid.minor.y = element_line(colour = "#71716F", linewidth = 0.5),
        plot.background = element_rect(colour = "white", fill = "white"),
        panel.background = element_rect(colour = "white", fill = "white"),
        plot.caption = element_text(hjust = 0, colour = "#71716F", size = 8, margin = margin(t = 20)),
        axis.text = element_text(colour = "#71716F"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.caption.position = "plot")

# Plot 2 ------------------------------------------------------------------

data2 <- bytes_total %>% 
  select(date, client, p50) %>% 
  mutate(date = ymd(date)) %>% 
  pivot_wider(names_from = client, values_from = p50) %>% 
  mutate(difference = desktop - mobile) %>% 
  drop_na()

ggplot(data = data3, 
       mapping = aes(x = date,
                     y = difference)) +
  geom_line(colour = "#40b1d6") +
  geom_hline(yintercept = 0) +
  scale_x_date(breaks = seq(ymd("2011-01-01"), ymd("2022-01-01"), by = "1 year"),
               labels = 2011:2022,
               limits = ymd(c("2010-11-01", "2022-10-01"))) +
  scale_y_continuous(breaks = seq(-200, 600, 100),
                     labels = c(format(seq(-200, 500, 100), big.mark=","), "600 KB"),
                     minor_breaks = NULL,
                     limits = c(-200, 700), 
                     expand = c(0, 0)) +
  labs(title = "Difference in average page weight for mobile vs desktop", 
       caption = "Source: HTTPArchive | N. Rennie | #TidyTuesday") +
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgrey", linewidth = 0.3),
        panel.grid.minor.y = element_line(colour = "#71716F", linewidth = 0.5),
        plot.background = element_rect(colour = "white", fill = "white"),
        panel.background = element_rect(colour = "white", fill = "white"),
        plot.caption = element_text(hjust = 0, colour = "#71716F", size = 8, margin = margin(t = 20)),
        axis.text = element_text(colour = "#71716F"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.caption.position = "plot")


# Plot 3 ------------------------------------------------------------------

data3 <- data2 %>% 
  mutate(dom = mday(date)) %>% 
  filter(dom == 1)

ggplot(data = data3, 
       mapping = aes(x = date,
                     y = difference)) +
  geom_line(colour = "#40b1d6") +
  geom_hline(yintercept = 0) +
  scale_x_date(breaks = seq(ymd("2011-01-01"), ymd("2022-01-01"), by = "1 year"),
               labels = 2011:2022,
               limits = ymd(c("2010-11-01", "2022-10-01"))) +
  scale_y_continuous(breaks = seq(-200, 600, 100),
                     labels = c(format(seq(-200, 500, 100), big.mark=","), "600 KB"),
                     minor_breaks = NULL,
                     limits = c(-200, 700), 
                     expand = c(0, 0)) +
  labs(title = "Difference in average page weight for mobile vs desktop", 
       caption = "Source: HTTPArchive | N. Rennie | #TidyTuesday") +
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgrey", linewidth = 0.3),
        panel.grid.minor.y = element_line(colour = "#71716F", linewidth = 0.5),
        plot.background = element_rect(colour = "white", fill = "white"),
        panel.background = element_rect(colour = "white", fill = "white"),
        plot.caption = element_text(hjust = 0, colour = "#71716F", size = 8, margin = margin(t = 20)),
        axis.text = element_text(colour = "#71716F"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.caption.position = "plot")


# Plot 4 ------------------------------------------------------------------

ts3 <- ts(data3$difference, start = c(2011, 6), end = c(2022, 10), frequency = 12)
cpts <- cpt.mean(data = ts3, method = "AMOC")
cpt <- cpts@cpts

ggplot() +
  geom_hline(yintercept = 0) +
  geom_rect(aes(xmin = ymd("2011-06-01"), 
                xmax = ymd(data3$date[cpt[1]]),
                ymin = -200,
                ymax = 600),
            fill = alpha("#40b1d6", 0.5)) +
  geom_rect(aes(xmin = ymd(data3$date[cpt[1]]),
                xmax = ymd("2022-10-01"),
                ymin = -200,
                ymax = 600),
            fill = alpha("#d3d3d3", 0.5)) +
  geom_line(data = data3, 
            mapping = aes(x = date,
                          y = difference),
            colour = "#23698c") +
  annotate("text", x = ymd(data3$date[cpt[1]]),
           y = 50,
           label = format(ymd(data3$date[cpt[1]]), "%B %Y"),
           fontface = "bold", 
           colour = "#23698c") +
  scale_x_date(breaks = seq(ymd("2011-01-01"), ymd("2022-01-01"), by = "1 year"),
               labels = 2011:2022,
               limits = ymd(c("2010-11-01", "2022-10-01"))) +
  scale_y_continuous(breaks = seq(-200, 600, 100),
                     labels = c(format(seq(-200, 500, 100), big.mark=","), "600 KB"),
                     minor_breaks = NULL,
                     limits = c(-200, 700), 
                     expand = c(0, 0)) +
  labs(title = "Difference in average page weight for mobile vs desktop", 
       caption = "Source: HTTPArchive | N. Rennie | #TidyTuesday") +
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgrey", linewidth = 0.3),
        panel.grid.minor.y = element_line(colour = "#71716F", linewidth = 0.5),
        plot.background = element_rect(colour = "white", fill = "white"),
        panel.background = element_rect(colour = "white", fill = "white"),
        plot.caption = element_text(hjust = 0, colour = "#71716F", size = 8, margin = margin(t = 20)),
        axis.text = element_text(colour = "#71716F"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.caption.position = "plot")

# save gif
gg_playback(
  name = file.path("2022", "2022-11-15","20221115.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)


