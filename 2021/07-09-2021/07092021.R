library(tidyverse)
library(extrafont)
library(patchwork)
library(rcartocolor)

tuesdata <- tidytuesdayR::tt_load('2021-09-07')

results <- tuesdata$results
races <- tuesdata$races
constructors <- tuesdata$constructors

plot_data <- left_join(left_join(filter(results, positionText == 1), races, by="raceId"), constructors, by="constructorId")
mean_data <- plot_data %>% group_by(year) %>% summarise(mean=mean(as.numeric(milliseconds)))
plot_data2 <- plot_data %>% 
  filter(name.x %in% c("Monaco Grand Prix", "British Grand Prix", "Italian Grand Prix", "French Grand Prix", "German Grand Prix")) 

p1 <- ggplot() +
  geom_line(mean_data, mapping=aes(x=year, y=mean/1000/60/60), colour="white", size=1.5) +
  geom_point(plot_data, mapping=aes(x=year, y=(((as.numeric(milliseconds)/1000)/60)/60), colour=nationality)) +
  labs(x="", y="Winning Time (hours)\n", title="Formula 1 Winning Times", 
       subtitle = "There was a sharp drop in winning times in the late 1950s, and they continued to\ndecrease until the 1970s where winning times have since levelled off. Since 2010\nthere has been a notable increase in the number of slower winning times. British\nmanufacturers dominated between 1960 and 2000, but have produced few\nwinners since then.") +
  scale_colour_carto_d(name = "Nationality of Constructor", palette = "Prism") +
  guides(colour = guide_legend(title.position = "top")) +
  theme(plot.background = element_rect(fill = "#192841", colour="#192841"),
        panel.background = element_rect(fill = "#192841", colour="#192841"),
        legend.background = element_rect(fill = "#192841"),
        strip.background =element_rect(fill="#192841"),
        strip.text = element_text(colour = '#ffc200', family="Candara", size=12),
        legend.key = element_rect(fill = "#192841", colour="#192841"), 
        legend.text =  element_text(colour = '#ffc200', size=12, family="Candara Light"),
        legend.title =  element_text(colour = '#ffc200', size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = '#ffc200', size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = '#ffc200', size=14, hjust = 0, family="Candara Light"),
        plot.caption = element_text(colour = '#ffc200', size=12, hjust = 1, family="Candara Light"),
        legend.position="bottom",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = '#ffc200', size=10, hjust = 0.5, family="Candara Light"),
        axis.text = element_text(colour = '#ffc200', size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour=alpha('#ffc200', 0.2)))
p1


p2 <- ggplot(plot_data2, aes(x=year, y=(((as.numeric(milliseconds)/1000)/60)/60), fill=name.x, group=name.x)) +
  geom_area(aes(group=name.x), color=NA) +
  scale_colour_carto_d(name = "", palette = "Prism") +
  facet_grid(~name.x) +
  scale_x_continuous(breaks=c(1980, 2020)) +
  labs(x="", y="Winning Time (hours)\n", subtitle="", caption="N.Rennie | Data: Ergast API") +
  theme(plot.background = element_rect(fill = "#192841", colour="#192841"),
        panel.background = element_rect(fill = "#192841", colour="#192841"),
        legend.background = element_rect(fill = "#192841"),
        strip.background =element_rect(fill="#192841"),
        strip.text = element_text(colour = '#ffc200', family="Candara", size=12),
        legend.key = element_rect(fill = "#192841", colour="#192841"), 
        legend.text =  element_text(colour = '#ffc200', size=12, family="Candara Light"),
        legend.title =  element_text(colour = '#ffc200', size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = '#ffc200', size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = '#ffc200', size=14, hjust = 0, family="Candara Light"),
        plot.caption = element_text(colour = '#ffc200', size=12, hjust = 1, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = '#ffc200', size=10, hjust = 0.5, family="Candara Light"),
        axis.text = element_text(colour = '#ffc200', size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour=alpha('#ffc200', 0.2)))
p2          

#join plots
p <-  p1 + p2 + plot_layout(ncol = 1) &
  theme(panel.background = element_rect(fill = "#192841", colour="#192841"),
        plot.background = element_rect(fill = "#192841", colour="#192841"))
p
