library(tidyverse)
library(camcorder)
library(showtext)
library(usefunc)
library(ggimage)

# load fonts
font_add_google("Tangerine", "tangerine")
font_add_google("Arsenal", "arsenal")
showtext_auto()

# get data
yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

# data wrangling
plot_data <- yarn %>% 
  filter(yarn_weight_name == "Aran", 
         rating_average == 5, 
         discontinued == FALSE) %>% 
  mutate(name = paste0(name, " (", yarn_company_name, "), ", yardage, " yards")) %>% 
  select(name, yardage) %>% 
  arrange(desc(yardage)) %>% 
  slice_max(yardage, n = 5) %>% 
  mutate(name = factor(name, levels = rev(name)))

# start recording
gg_record(
  dir = file.path("2022", "2022-10-11", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 4, # width of saved image
  height = 5, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# subtitle
st <- str_wrap_break("There are 895 Aran weighted yarns with an average rating of 5 out of 5. Of those 895, the five with the highest yardage are shown below.\n\nN. Rennie | Data: ravelry.com | #TidyTuesday", 100)

# read in image
img <- paste0(here::here(), "/2022/2022-10-11/wool.png")

# plot
ggplot(plot_data) +
  geom_segment(aes(x = 0, 
                   xend = yardage, 
                   y = name,
                   yend = name), 
               size = 0.3) +
  geom_image(aes(x = 0,
                 y = (1:5)+0.175, 
                 image = img), 
             size = 0.07) +
  geom_text(aes(x = yardage + 100, 
                y = name, 
                label = name), 
            hjust = 0, 
            family = "arsenal", 
            lineheight = 0.4, 
            size = 8) +
  labs(title = "{ravelRy}: Aran Weighted Yarn", 
       subtitle = st) +
  scale_x_continuous(limits = c(0, 3700)) +
  theme(plot.background = element_rect(fill = "#FEFBEA", colour = "#FEFBEA"), 
        panel.background = element_rect(fill = "#FEFBEA", colour = "#FEFBEA"), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(family = "tangerine",
                                  size = 60, 
                                  hjust = 0.5), 
        plot.subtitle = element_text(family = "arsenal",
                                    size = 20, 
                                    hjust = 0.5, 
                                    lineheight = 0.5, 
                                    margin = margin(t = 10)))
  
# save gif
gg_playback(
  name = file.path("2022", "2022-10-11","20221011.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)


