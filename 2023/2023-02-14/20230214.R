library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(patchwork)
library(nrBrand)
library(gghighlight)

# load fonts
font_add_google("Fraunces", "Fraunces")
font_add_google("Commissioner", "Commissioner")
showtext_auto()

# load data
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

# data wrangling
plot_data <- age_gaps |> 
  mutate(rel_type = case_when(
    (character_1_gender == "woman" & character_2_gender == "man") ~ "Older Woman, Younger Man",
    (character_1_gender == "man" & character_2_gender == "woman") ~ "Older Man, Younger Woman",
    (character_1_gender == "woman" & character_2_gender == "woman") ~ "Both Women",
    (character_1_gender == "man" & character_2_gender == "man") ~ "Both Men" 
  )) |> 
  select(release_year, age_difference, rel_type) 

# start recording
gg_record(
  dir = file.path("2023", "2023-02-14", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# subtitle
social = "<span style='font-family:\"Font Awesome 6 Brands\";color:#E30B5C;'>&#xf099;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>@nrennie35</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#E30B5C;'>&#xf4f6;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>fosstodon.org/@nrennie</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#E30B5C;'>&#xf09b;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>nrennie</span><span style='color:white;'>..</span>"
st = "Age gaps beween characters in Hollywood films have generally been decreasing over time. Note that this<br>data includes gender, which always contain the values *man* or *woman*. These values appear to indicate<br>how the characters in each film identify. Some of these values do not match how the actor identifies.<br><br>Data: Data Is Plural"
cap = glue("{social}<br><br>{st}")

# plot
p1 <- ggplot(data = plot_data,
       mapping = aes(x = release_year,
                     y = age_difference)) +
  geom_point(colour = alpha("#508080", 0.5)) +
  geom_smooth(colour = "#E30B5C", se = FALSE) +
  labs(y = "Age gap (years)") +
  theme_minimal()

# faceted plot
p2 <- ggplot(data = plot_data,
       mapping = aes(x = release_year,
                     y = age_difference, 
                     colour = rel_type)) +
  geom_point(colour = alpha("#E30B5C", 0.5),
             size = 0.3) +
  gghighlight(use_direct_label = F) +
  facet_wrap(~rel_type, ncol = 2, nrow = 2,
             labeller = labeller(rel_type = label_wrap_gen(18))) +
  labs(y = "Age gap (years)") +
  theme_minimal()

# join with patchwork
p1 + p2 +
  plot_annotation(title = "Hollywood Age Gaps",
                  subtitle = cap) &
  theme(text = element_text(family = "Commissioner",
                            colour = "#2F4F4F"),
        plot.subtitle = element_markdown(size = 26,
                                         lineheight = 0.4,
                                         hjust = 0,
                                         colour = "#2F4F4F",
                                         margin = unit(c(0, 0, -0.5, 0), "cm")),
        plot.tag.position = c(0.01, 0.72),
        plot.margin = margin(0, 10, 10, 10),
        plot.title = element_text(family = "Fraunces",
                                  size = 60,
                                  colour = "#2F4F4F",
                                  margin = unit(c(0.5, 0, 0.5, 0), "cm")),
        plot.title.position = "plot",
        axis.text = element_text(size = 24, vjust = 2, colour = "#2F4F4F"),
        axis.title.y = element_text(size = 24, vjust = 2, colour = "#2F4F4F"),
        strip.text = element_text(size = 24, lineheight = 0.4, colour = "#2F4F4F"),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#F0F5F5", colour = "#F0F5F5"),
        panel.background = element_rect(fill = "#F0F5F5", colour = "#F0F5F5"))

# save gif
gg_playback(
  name = file.path("2023", "2023-02-14","20230214.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#F0F5F5"
)
