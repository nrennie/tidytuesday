library(tidyverse)
library(ggbump)
library(rcartocolor)
library(showtext)
library(cowplot)
library(magick)

# load data
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

# load fonts
font_add_google(name = "Ubuntu", family = "ubuntu")
font_add_google(name = "Frijole", family = "frijole")
showtext_auto()

# function to wrap strings and break lines
trimmer <- function(x, break_limit) {     
  sapply(strwrap(x, break_limit, simplify=FALSE), paste, collapse="\n")      
}

# prep data
plot_data <- breed_rank_all %>%
  filter(`2020 Rank` %in% 1:10) %>% 
  pivot_longer(cols = 2:9) %>%
  mutate(year = as.numeric(str_sub(name, 1, 4)))

# plot
p <- ggplot() +
  geom_bump(data=filter(plot_data, value <= 10, Breed != "Dachshunds"), 
            mapping=aes(x = year, y = value, colour=Breed), 
            size = 1) +
  geom_point(data=filter(plot_data, value <= 10), 
             mapping=aes(x = year, y = value, colour=Breed), 
             size = 3.5) + 
  geom_text(data = filter(plot_data, year == 2020), 
            mapping = aes(x = 2020.2, y = value, label = str_wrap(Breed, 20), colour = Breed), 
            hjust = 0, family="ubuntu") +
  labs(x="", 
       y="", 
       title = "Most Popular Dog Breeds", 
       tag = trimmer("Labrador retrievers have been consistently in the top spot for the most popular dog breed. French bulldogs have made a staggering climb up the ranks to become second most popular in 2020.\n\n\nN. Rennie | Data: American Kennel Club\n\nImage: Bjorn Agerbeek on Unsplash", 40)) +
  scale_y_reverse(breaks = c(1:13)) +
  scale_x_continuous(breaks = c(2013:2020), limits = c(2013, 2026)) +
  scale_colour_carto_d(palette = "Prism") +
  theme(plot.background = element_rect(fill = "gray90", colour="gray90"),
        panel.background = element_rect(fill = "gray90", colour="gray90"),
        legend.position = "none", 
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 6, 0.5, 1), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text =  element_text(colour = "black", size=10, hjust = 0.5, family="ubuntu"),
        plot.title = element_text(colour = "black", size=28, hjust = 3.5, family="frijole", 
                                  margin = margin(10, 0, 20, 0)),
        plot.tag.position = c(1.05, 0.35),
        plot.tag = element_text(colour = "black", size=12, hjust = 0.5, family="ubuntu"))
p

# add image
img <- image_read("dog.png")
q <- ggdraw() + 
  draw_plot(p) +
  draw_image(img, 0.31, 0.2, scale=0.28)
q


