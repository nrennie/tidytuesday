library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(patchwork)
library(nrBrand)
library(png)

# load data
plot_data <- age_gaps |> 
  filter(movie_name == "Love Actually")

#start recording
gg_record(
  dir = file.path("2023", "2023-02-14", "recording_2"), # where to save the recording
  device = "png", # device to use to save images
  width = 3, # width of saved image
  height = 4.5, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# data wrangling
plot_data <- age_gaps |> 
  mutate(rel_type = case_when(
    (character_1_gender == "woman" & character_2_gender == "man") ~ "Older Woman, Younger Man",
    (character_1_gender == "man" & character_2_gender == "woman") ~ "Older Man, Younger Woman",
    (character_1_gender == "woman" & character_2_gender == "woman") ~ "Both Women",
    (character_1_gender == "man" & character_2_gender == "man") ~ "Both Men" 
  )) |> 
  select(release_year, age_difference, rel_type, movie_name) 

title <- "<span style='color:#cd2129;'>**love**</span>actually"

# plot
p <- ggplot(data = plot_data,
       mapping = aes(x = release_year,
                     y = age_difference, 
                     colour = rel_type)) +
  geom_point(colour = alpha("lightgrey", 0.8),
             size = 0.4) +
  geom_point(data = filter(plot_data, movie_name == "Love Actually"),
             colour = alpha("#cd2129", 0.9),
             size = 0.7) +
  facet_wrap(~rel_type, ncol = 2, nrow = 2,
             labeller = labeller(rel_type = label_wrap_gen(18))) +
  labs(y = "Age gap (years)", 
       title = title) +
  theme_minimal() +
  theme(plot.title = element_markdown(hjust = 0.5,
                                      size = 110,
                                      margin = margin(t = 30, b = 30)),
        plot.margin = margin(10, 0, 20, 0),
        legend.position = "none",
        axis.text = element_text(size = 24, colour = "black", hjust = 1.5),
        axis.title = element_blank(),
        strip.text = element_text(size = 24, lineheight = 0.4),
        panel.spacing = unit(2, "lines"),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"))
p

# add ribbon
bow <- readPNG("2023/2023-02-14/bow.png", native = TRUE, info = TRUE)
p +  inset_element(p = bow,
                   left = -0.14,
                   bottom = -0.1,
                   right = 1.08,
                   top = 1.1) &
  theme(plot.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"))

# save gif
gg_playback(
  name = file.path("2023", "2023-02-14","20230214_love_actually.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)

ggsave("2023/2023-02-14/20230214_love_actually.png", height = 4.5, width = 3, units = "in", bg = "white")

