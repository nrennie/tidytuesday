library(tidyverse)
library(treemapify)
library(showtext)
library(ggsci)

# load data
tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate

# add fonts
font_add_google(name = "Bebas Neue", family = "bebas")
showtext_auto()

# prep data
plot_data <- chocolate %>%
  group_by(country_of_bean_origin) %>% 
  summarise(n = n(), 
            rating = mean(rating)) %>%
  arrange(desc(n))
plot_data

# treemap plot
ggplot(plot_data, aes(area = n, fill = rating, label = country_of_bean_origin)) +
  geom_treemap(colour = "#452d28") +
  geom_treemap_text(fontface = "italic", 
                    family="bebas",
                    colour = "#452d28", 
                    place = "centre",
                    grow = TRUE, 
                    padding.x = grid::unit(4, "mm"),
                    padding.y = grid::unit(4, "mm")) +
  scale_fill_material(palette="brown", name="", 
                      limits=c(2.8, 3.6), 
                      breaks=c(2.8, 3.6), 
                      labels=c("<--\nLower rating", "-->\nHigher rating"), 
                      guide = guide_colourbar(title.position = "top")) +
  labs(title = "Where do cacao beans come from?", 
       subtitle = "\nCacoa beans from countries which are used by a larger number of manufacturers tend to result in higher rated\nchocolate. The exception is blended beans which are commonly used but score lower. \n\n N. Rennie | Data: Flavors of Cacao\n\n") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#452d28", colour="#452d28"),
        panel.background = element_rect(fill = "#452d28", colour="#452d28"),
        legend.position = "bottom",
        plot.title = element_text(colour = '#b29e97', family="bebas", face = "bold", size=22),
        plot.subtitle = element_text(colour = '#b29e97', family="bebas", size=14),
        legend.text = element_text(colour = '#b29e97', family="bebas", size=12), 
        legend.title = element_text(colour = '#b29e97', family="bebas", size=12), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))






