library(tidyverse)
library(magick)
library(grid)
library(extrafont)
library(sf)
library(cowplot)
library(rnaturalearthdata)
library(rnaturalearth)

tuesdata <- tidytuesdayR::tt_load('2021-10-12')
farmed <- tuesdata$`aquaculture-farmed-fish-production`
captured_vs_farmed <- tuesdata$`capture-fisheries-vs-aquaculture`
captured <- tuesdata$`capture-fishery-production`
consumption <- tuesdata$`fish-and-seafood-consumption-per-capita`
stock <- tuesdata$`fish-stocks-within-sustainable-levels`
fishery <- tuesdata$`global-fishery-catch-by-sector`
production <- tuesdata$`seafood-and-fish-production-thousand-tonnes`

#### PREPARE COMMON OBJECTS ####
caption_rect <- rectGrob(
  x = unit(0, "in"),
  y = unit(0.25, "in") ,
  width = unit(8, "in"),
  height = unit(0.25, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "black", alpha = 0.4)
)


#### FRAME 1 ####
img <- image_read("bg_image_colour.jpg") %>%
  image_resize("1200x800") 
rect <- rectGrob(
  x = unit(0, "in"),
  y = unit(1, "npc") - unit(0.5, "in"),
  width = unit(4.5, "in"),
  height = unit(1, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "black", alpha = 0.4)
)
text_rect <- rectGrob(
  x = unit(5, "in"),
  y = unit(1, "npc") - unit(1.1, "in"),
  width = unit(2.5, "in"),
  height = unit(2.8, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "black", alpha = 0.4)
)
circle1 <- circleGrob(x = 0.08, y = 0.57, r = 0.04,
                        gp = gpar(fill = "#54808b"))
circle2 <- circleGrob(x = 0.08, y = 0.42, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
circle3 <- circleGrob(x = 0.08, y = 0.27, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
circle4 <- circleGrob(x = 0.08, y = 0.12, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
p <- ggdraw() + 
  draw_image(img) + 
  draw_grob(rect) + 
  draw_grob(caption_rect) +
  draw_grob(text_rect) +
  draw_grob(circle1) +
  draw_grob(circle2) +
  draw_grob(circle3) +
  draw_grob(circle4) +
  draw_label(x=0.08, y=0.57, hjust=0.5, vjust=0.5, "1", 
             color = "black", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.42, hjust=0.5, vjust=0.5, "2", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.27, hjust=0.5, vjust=0.5, "3", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.12, hjust=0.5, vjust=0.5, "4", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.07, y=0.84, hjust=0, vjust=1, "GLOBAL SEAFOOD", 
             color = "#54808b", fontface="bold", size = 26, fontfamily="Berlin Sans FB") +
  draw_label(x=0.02, y=0.02, hjust=0, "N. Rennie | Data: OurWorldinData.org | Background: Jakob Owens @ Unsplash", 
             color = "#a9bfc4", size = 10, fontfamily="Segoe UI") +
  draw_label(x=0.65, y=0.7, hjust=0, "The world now produces more\nthan 155 million tonnes of\nseafood each year.\n\n\nIncreasing pressures on fish\npopulations mean one-third of\nglobal fish stocks are over-\nexploited – this has increased\nfrom 10% in the 1970s.", 
             color = "#a9bfc4", size = 11, fontfamily="Segoe UI", vjust=1) 

p
ggsave(p, filename="12102021_f1.jpg", height=1600, width=2398, unit="px")


#### FRAME 2 ####
img <- image_read("bg_image_bw.jpg") %>%
  image_resize("1200x800")
circle1 <- circleGrob(x = 0.08, y = 0.57, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
circle2 <- circleGrob(x = 0.08, y = 0.42, r = 0.04,
                      gp = gpar(fill = "#54808b"))
circle3 <- circleGrob(x = 0.08, y = 0.27, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
circle4 <- circleGrob(x = 0.08, y = 0.12, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
text_rect <- rectGrob(
  x = unit(0.5, "in"),
  y = unit(1, "npc") - unit(0.5, "in"),
  width = unit(7, "in"),
  height = unit(1, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "black", alpha = 0.4)
)
#geom area
colnames(production) <- c("Entity","Code", "Year", "Pelagic Fish", "Crustaceans", "Cephalopods", "Demersal Fish", "Freshwater Fish", "Other Molluscs", "Other Marine Fish" )
plot_data <- production %>% 
  pivot_longer(cols=4:10, names_to = "type", values_to = "tonnes") %>%
  group_by(Year, type) %>%
  summarise(sum = sum(tonnes, na.rm=T))
set.seed(1234)
p_inset <- ggplot(plot_data, aes(x=Year, y=sum, fill=type)) + 
  geom_area() + 
  coord_cartesian(expand = F) +
  scale_y_continuous(label = scales::label_number_si()) +
  scale_fill_manual("", values=sample(c('#f6eff7','#d0d1e6','#a6bddb','#67a9cf','#3690c0','#02818a','#016450'), size=7, replace = F)) +
  labs(x="", y="Tonnes per year\n") + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
        plot.background = element_rect(fill = alpha("black", 0.4), colour="transparent"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#a9bfc4", size=10, hjust = 0, family="Segoe UI"),
        legend.title = element_blank(),
        legend.position="right",
        plot.margin = unit(c(0.3, 3.5, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#a9bfc4", size=10, hjust = 0.5, family="Segoe UI"),
        axis.text = element_text(colour = "#a9bfc4", size=10, hjust = 0.5, family="Segoe UI"),
        axis.ticks = element_line(colour="#a9bfc4"),
        panel.grid.major = element_line(colour="#a9bfc4"),
        panel.grid.minor = element_blank()) 
p_inset
p <- ggdraw() + 
  draw_image(img) + 
  draw_grob(caption_rect) +
  draw_grob(text_rect) +
  draw_grob(circle1) +
  draw_grob(circle2) +
  draw_grob(circle3) +
  draw_grob(circle4) +
  draw_plot(p_inset, .21, .15, .73, .5) +
  draw_label(x=0.08, y=0.57, hjust=0.5, vjust=0.5, "1", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.42, hjust=0.5, vjust=0.5, "2", 
             color = "black", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.27, hjust=0.5, vjust=0.5, "3", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.12, hjust=0.5, vjust=0.5, "4", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.02, y=0.02, hjust=0, "N. Rennie | Data: OurWorldinData.org | Background: Jakob Owens @ Unsplash", 
             color = "#a9bfc4", size = 10, fontfamily="Segoe UI") +
  draw_label(x=0.1, y=0.85, hjust=0, "Global seafood and fish production is higher than it has ever been with 681,822,807 tonnes\nproduced in 2013. In 1961, that value was only 173,034,558. The largest growth in production\nhas been seen in freshwater fish, especially since 2000.", 
             color = "#a9bfc4", size = 11, fontfamily="Segoe UI", vjust=1) 

p
ggsave(p, filename="12102021_f2.jpg", height=1600, width=2398, unit="px")

#### FRAME 3 ####
img <- image_read("bg_image_bw.jpg") %>%
  image_resize("1200x800")
circle1 <- circleGrob(x = 0.08, y = 0.57, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
circle2 <- circleGrob(x = 0.08, y = 0.42, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
circle3 <- circleGrob(x = 0.08, y = 0.27, r = 0.04,
                      gp = gpar(fill = "#54808b"))
circle4 <- circleGrob(x = 0.08, y = 0.12, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
text_rect <- rectGrob(
  x = unit(0.5, "in"),
  y = unit(1, "npc") - unit(0.5, "in"),
  width = unit(7, "in"),
  height = unit(1, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "black", alpha = 0.4)
)
#map
plot_data <- consumption %>%
  filter(Year == 2017)
colnames(plot_data) <- c(colnames(plot_data)[1:3], "consumption")
world <- ne_countries(scale = "medium", returnclass = "sf")
plot_data$iso_a3 <- plot_data$Code
k <- left_join(data.frame(iso_a3=world$iso_a3), plot_data, by = "iso_a3")
world$consumption <- k$consumption
p_inset <- ggplot() +
  geom_sf(data = world, aes(fill = consumption), colour=NA) +
  labs() +
  scale_fill_gradient("kg/capita\n", high="#02818a", low="#a6bddb", breaks=c(25,50,75), labels=c("  25", "  50", "  75")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
        plot.background = element_rect(fill = alpha("black", 0.4), colour="transparent"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#a9bfc4", size=10, hjust = 1, family="Segoe UI"),
        legend.title = element_text(colour = "#a9bfc4", size=10, hjust = 0.5, family="Segoe UI"),
        legend.position="right",
        plot.margin = unit(c(0.3, 1.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#a9bfc4", size=10, hjust = 0.5, family="Segoe UI"),
        axis.text = element_text(colour = "#a9bfc4", size=10, hjust = 0.5, family="Segoe UI"),
        axis.ticks = element_line(colour="#a9bfc4"),
        panel.grid.major = element_line(colour=alpha("#a9bfc4", 0.4)),
        panel.grid.minor = element_blank()) 
p_inset
p <- ggdraw() + 
  draw_image(img) + 
  draw_grob(caption_rect) +
  draw_grob(text_rect) +
  draw_grob(circle1) +
  draw_grob(circle2) +
  draw_grob(circle3) +
  draw_grob(circle4) +
  draw_plot(p_inset, .21, .15, .73, .5) +
  draw_label(x=0.08, y=0.57, hjust=0.5, vjust=0.5, "1", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.42, hjust=0.5, vjust=0.5, "2", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.27, hjust=0.5, vjust=0.5, "3", 
             color = "black", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.12, hjust=0.5, vjust=0.5, "4", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.02, y=0.02, hjust=0, "N. Rennie | Data: OurWorldinData.org | Background: Jakob Owens @ Unsplash", 
             color = "#a9bfc4", size = 10, fontfamily="Segoe UI") +
  draw_label(x=0.1, y=0.85, hjust=0, "In 2017, average annual consumption of fish and seafood was 19.24kg per capita, up from\n11.62kg per capita in 1961. Iceland recorded the highest consumption per capita in 2017,\nat 90.7 kg per year.", 
             color = "#a9bfc4", size = 11, fontfamily="Segoe UI", vjust=1) 

p
ggsave(p, filename="12102021_f3.jpg", height=1600, width=2398, unit="px")


#### FRAME 4 ####
img <- image_read("bg_image_bw.jpg") %>%
  image_resize("1200x800")
circle1 <- circleGrob(x = 0.08, y = 0.57, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
circle2 <- circleGrob(x = 0.08, y = 0.42, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
circle3 <- circleGrob(x = 0.08, y = 0.27, r = 0.04,
                      gp = gpar(fill = "black", alpha=0.4))
circle4 <- circleGrob(x = 0.08, y = 0.12, r = 0.04,
                      gp = gpar(fill = "#54808b"))
text_rect <- rectGrob(
  x = unit(0.5, "in"),
  y = unit(1, "npc") - unit(0.5, "in"),
  width = unit(7, "in"),
  height = unit(1, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "black", alpha = 0.4)
)
#geom line
p_inset <- ggplot(filter(stock, Entity == "World"), aes(x=Year, y=`Share of fish stocks that are overexploited`)) +
  geom_line(colour="#54808b") +
  geom_point(colour="#a9bfc4") + 
  coord_cartesian(expand = F) +
  scale_y_continuous(limits=c(0,100)) +
  labs(x="", y="% of fish stocks that\nare over-exploited\n") + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
        plot.background = element_rect(fill = alpha("black", 0.4), colour="transparent"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#a9bfc4", size=10, hjust = 0, family="Segoe UI"),
        legend.title = element_blank(),
        legend.position="right",
        plot.margin = unit(c(0.3, 0.8, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#a9bfc4", size=10, hjust = 0.5, family="Segoe UI"),
        axis.text = element_text(colour = "#a9bfc4", size=10, hjust = 0.5, family="Segoe UI"),
        axis.ticks = element_line(colour="#a9bfc4"),
        panel.grid.major = element_line(colour=alpha("#a9bfc4", 0.4)),
        panel.grid.minor = element_blank()) 
p_inset
p <- ggdraw() + 
  draw_image(img) + 
  draw_grob(caption_rect) +
  draw_grob(text_rect) +
  draw_grob(circle1) +
  draw_grob(circle2) +
  draw_grob(circle3) +
  draw_grob(circle4) +
  draw_plot(p_inset, .21, .15, .73, .5) +
  draw_label(x=0.08, y=0.57, hjust=0.5, vjust=0.5, "1", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.42, hjust=0.5, vjust=0.5, "2", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.27, hjust=0.5, vjust=0.5, "3", 
             color = "#54808b", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.08, y=0.12, hjust=0.5, vjust=0.5, "4", 
             color = "black", fontface="bold", size = 20, fontfamily="Berlin Sans FB") +
  draw_label(x=0.02, y=0.02, hjust=0, "N. Rennie | Data: OurWorldinData.org | Background: Jakob Owens @ Unsplash", 
             color = "#a9bfc4", size = 10, fontfamily="Segoe UI") +
  draw_label(x=0.1, y=0.84, hjust=0, "The share of fish stocks which are over-exploited has been steadily increasing since the\n1970s. In some areas of the world, the share of over-exploited stock exceeds 60%.", 
             color = "#a9bfc4", size = 11, fontfamily="Segoe UI", vjust=1) 

p
ggsave(p, filename="12102021_f4.jpg", height=1600, width=2398, unit="px")




