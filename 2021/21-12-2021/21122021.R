library(tidyverse)
library(emojifont)
library(cowplot)
library(magick)

tuesdata <- tidytuesdayR::tt_load('2021-12-21')
starbucks <- tuesdata$starbucks

#select coffees
coffees <- c("Cappuccino",
             "Flat White", 
             "Caffè Latte", 
             "Caffè Mocha", 
             "English Breakfast Black Brewed Tea",
             "Hot Chocolate", 
             "Iced Coffee", 
             "Cold Brewed Coffee", 
             "Espresso - Caffè Americano"
)

labels1 <- data.frame(x=1:4, y=rep(10, 4), label=c("Short", "Tall", "Grande", "Venti"))

plot_data <- starbucks %>% 
  filter(whip == 0, 
         product_name %in% coffees, 
         size %in% c("short", "tall", "grande", "venti")) %>%
  select(product_name, size, caffeine_mg, milk) %>%
  group_by(product_name, size) %>% 
  summarise(caffeine = mean(caffeine_mg)) %>% 
  mutate(size = factor(size, levels=c("short", "tall", "grande", "venti")), 
         label = fontawesome('fa-coffee')) 
  
p <- ggplot() +
  geom_text(data=plot_data, mapping=aes(x=size, y=reorder(product_name,caffeine), 
                                        label=label, size=size, colour=caffeine), 
            family='fontawesome-webfont', vjust=0) +
  geom_text(data=labels1, mapping=aes(x=x, y=y, label=label), 
            fontface="italic", colour = "#00704A", size=3.5) +
  scale_size_manual("", values=c(4, 6, 8, 10), guide="none") +
  scale_colour_gradient("Caffeine (mg)", high="#00704A", low="#abd1bc") +
  coord_fixed(xlim=c(1,4), ylim=c(-1,10.2)) +
  theme_void() +
  labs(title="What gives the biggest caffeine fix at Starbucks?", 
       caption = "N.Rennie | Data: Starbucks Coffee Company", 
       subtitle = "*based on whole milk with no whip") +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5, direction = "horizontal")) +
  theme(plot.background = element_rect(fill = "white", colour="white"),
        panel.background = element_rect(fill = "white", colour="white"),
        plot.title = element_text(colour = "#00704A", size=12, family="sans", hjust=1.1, vjust=0),
        plot.subtitle = element_text(colour = "#00704A", size=12, face="italic", family="sans", hjust=2.0,
                                     margin = margin(15,2,2,2)),
        plot.caption = element_text(colour = "#00704A", size=10, family="sans", hjust=1.6, 
                                    margin = margin(15,2,2,2)),
        legend.background = element_rect(fill = "white", colour="white"),
        legend.key = element_rect(fill = "white", colour="white"), 
        legend.title =  element_text(colour = "#00704A", size=10, family="sans"),
        legend.text =  element_text(colour = "#00704A", size=8, family="sans"),
        legend.position=c(0, 0.05),
        axis.text.y = element_text(colour = "#00704A", size=10, family="sans", hjust=1, face="italic"),
        plot.margin = unit(c(4, 1.2, 0.3, 0.3), "cm")) #top, right, bottom, left
p

#add logo
img <- image_read("logo.png")
q <- ggdraw() + 
  draw_plot(p) +
  draw_image(img, 0, 0.4, scale=0.2)
q






