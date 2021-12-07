library(tidyverse)
library(spiralize)
library(ComplexHeatmap) #devtools::install_github("jokergoo/ComplexHeatmap")
library(cowplot)
library(magick)

tuesdata <- tidytuesdayR::tt_load('2021-12-07')
spiders <- tuesdata$spiders

unique(spiders$family)

#check most common family
spiders %>%
  group_by(family) %>% 
  summarise(n=n()) %>%
  arrange(desc(n))

#prep data
plot_data <- spiders %>% 
  filter(family == "Agelenidae") %>%
  select(year) %>% 
  group_by(year) %>%
  summarise(n=n()) 
plot_data

#colour function
col_fun = circlize::colorRamp2(seq(0,120,by=20), rcartocolor::carto_pal(n=7, name="Magenta"))

#spiral plot
spiral_initialize(xlim=c(1750, 2022), polar_lines_gp = gpar(col = "white", lty = 3))
spiral_track(ylim=c(0, 120), background_gp = gpar(col = NA, fill = "grey95"))
spiral_bars(plot_data$year, plot_data$n, gp = gpar(fill = col_fun(plot_data$n), col = NA))

#add legend
lgd = Legend(title = "Number of publications", 
             at = c(0, 60, 120),
             col_fun = col_fun, 
             direction = "horizontal", 
             title_gp = gpar(fontsize = 12, fontfamily="sans", col = "#6c2167"),
             labels_gp = gpar(fontsize = 12, fontfamily="sans", col = "#6c2167"),
             legend_width = unit(60, "mm"),
             title_position = "topcenter")
draw(lgd, x = unit(0.5, "npc") , y=unit(-0.1, "npc"), 
     just = "center") 

#paragraph
s <- "Spiders in the Agelenidae family are more commonly known as funnel weaver or funnel web spiders due to the shape of their webs.\n\nThe first mention of such spiders is in 1757 by Clerck, where they were found across Europe, North Africa, Central Asia, and Russia amongst others."

#add labels 
img <- image_read("spiral.jpeg")
q <- ggdraw() + 
  draw_image(img, -0.2, 0.05, scale=1) +
  #title
  draw_label(x=0.78, y=0.75, hjust=0.5, 
             "Agelenidae", 
             color = "#6c2167", size = 34, fontfamily="serif") +
  #subtitle
  draw_label(x=0.78, y=0.65, hjust=0.5, 
             "Funnel Web Spider", 
             color = "#6c2167", size = 12, fontfamily="sans", fontface = "italic") +
  #paragraph
  draw_label(x=0.78, y=0.4, hjust=0.5, 
             str_wrap(s, width = 40), 
             color = "#6c2167", size = 12, fontfamily="sans") +
  #caption
  draw_label(x=0.78, y=0.05, hjust=0.5, 
             "N. Rennie | Data: World Spider Database", 
             color = "#6c2167", size = 10, fontfamily="sans") +
  #year labels
  draw_label(x=0.52, y=0.57, hjust=0.5, 
             "2021", 
             color = "#6c2167", size = 10, fontfamily="sans") +
  draw_label(x=0.36, y=0.53, hjust=0.5, 
             "1757", 
             color = "#6c2167", size = 10, fontfamily="sans")
q



