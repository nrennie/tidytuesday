library(tidyverse)
library(extrafont)
library(cowplot)
library(viridis) 
library(gganimate)
library(rsvg)
library(magick)

tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics
regions <- tuesdata$regions

#Dot plot of Team GB gold medals
gold_medals_GB <- filter(olympics, medal == "Gold" & season == "Summer" & noc == "GBR") %>% group_by(sport) %>% summarise(total_medals = n())
gold_medals_GB$sport <- factor(gold_medals_GB$sport, levels=gold_medals_GB$sport[order(gold_medals_GB$total_medals)])
gb_logo <- "./images/gb_logo.png"
gold_medals_GB$y_text <- rep(120,30)

p <- ggplot(gold_medals_GB, aes(x=sport, y=total_medals)) + 
  geom_point(col="#da0019", size=3) +  
  geom_text(aes(x=sport, y=y_text, label=total_medals), colour = "#da0019", size=4, hjust = 0.5, family="Gill Sans MT") +
  geom_segment(aes(x=sport, xend=sport, y=min(total_medals), yend=max(total_medals)), linetype="dashed", size=0.1, colour="#005085") +   # Draw dashed lines
  labs(title="TEAM GB: Going for Gold", 
       subtitle="Since the Summer Olympic Games began in 1896, TEAM\nGB have won 636 gold medals across 30 different sports*.\nRowing is easily thier best sport, winning 109 gold medals.", 
       caption="\nN. Rennie | Data: www.sports-reference.com\n*Data up to and including Rio 2016", 
       x="", y="Total number of gold medals") +  
  scale_y_continuous(breaks=c(25, 50, 75, 100)) +
  coord_flip() +
  theme(plot.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        panel.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        legend.position="none",
        axis.ticks.y = element_blank(),
        axis.text = element_text(colour = "#005085", size=10, hjust = 0.5, family="Gill Sans MT"),
        axis.title = element_text(colour = "#005085", size=12, hjust = 0.5, family="Gill Sans MT"),
        plot.title = element_text(colour = "#005085", size=28, hjust = 0, family="Gill Sans MT"),
        plot.subtitle = element_text(colour = "#005085", size=14, hjust = 0, family="Gill Sans MT"),
        plot.caption = element_text(colour = "#005085", size=12, hjust = 1, family="Gill Sans MT"),
        plot.margin = unit(c(0.6, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

q <- ggdraw() + 
  draw_plot(p) +
  draw_image(gb_logo, x = 0.16, y = 0.13, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.2) 
q

############################################################################################################################
############################################################################################################################

gold_medals <- filter(olympics, medal == "Gold" & season == "Summer") %>% group_by(year, noc) %>% modify_if(is.character, as.factor) %>% modify_if(is.double, as.factor) %>% tally() %>% ungroup() %>% group_by(noc) %>% mutate(csum = cumsum(n)) %>% ungroup() 
gold_medals$year <- as.numeric(as.character(gold_medals$year))

gold_medals_smooth <- gold_medals %>%
  group_by(noc) %>%
  mutate(csum2 = spline(x = year, y = csum, xout = year)$y) %>%
  group_by(year) %>%
  mutate(rank = min_rank(-csum2) * 1) %>%
  ungroup() %>%
  arrange(noc,year)

rings <- image_read_svg('https://upload.wikimedia.org/wikipedia/commons/5/5c/Olympic_rings_without_rims.svg', width=400)

p <- ggplot(gold_medals_smooth,aes(rank, group = noc, fill = as.factor(noc), color = as.factor(noc))) +
  annotation_raster(rings, ymin = 1500, ymax = 2500, xmin = -48, xmax = -40) +
  geom_tile(aes(y = csum2/2, height = csum2, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(noc, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = csum2, label = scales::comma(csum2)), hjust = 0, nudge_y = 200) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  scale_fill_viridis(discrete=TRUE, option = "B") +
  scale_color_viridis(discrete=TRUE, option = "B") +
  guides(color = "none", fill = "none") +      
  labs(title='Total Olympic Gold Medals: {as.integer(frame_time)}', x = "", y = "Cumulative number of gold medals", 
       caption = "\n\nN.Rennie | Data: www.sports-reference.com\n*Data up to and including Rio 2016",
       subtitle="Cumulative number of Olympic gold medals won by different\ncountries since 1896. The USA shot to the top of the gold\nmedal table in 1904 and have been there ever since.") +
  theme(plot.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        panel.background = element_rect(fill = "#f6f6f6", colour="#f6f6f6"),
        axis.text = element_text(colour = "#005085", size=10, hjust = 0.5, family="Gill Sans MT"),
        axis.title = element_text(colour = "#005085", size=12, hjust = 0.5, family="Gill Sans MT"),
        plot.title = element_text(colour = "#005085", size=28, hjust = 0, family="Gill Sans MT"),
        plot.subtitle = element_text(colour = "#005085", size=15, hjust = 0, family="Gill Sans MT"),
        plot.caption = element_text(colour = "#005085", size=12, hjust = 1, family="Gill Sans MT"),
        axis.ticks.y = element_blank(),  
        axis.text.y  = element_blank(),  
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_time(year)+
  enter_grow() +
  exit_shrink() +
  ease_aes('linear')

anim <- animate(p, fps = 8, width = 600, height = 800)
anim_save("27072021.gif", anim)




