library(tidyverse)
library(ggstream)
library(extrafont)
library(patchwork)
tuesdata <- tidytuesdayR::tt_load('2021-06-22')
parks <- tuesdata$parks

#process data
parks$city[which(parks$city == "Washington, DC")] <- "Washington, D.C."
cities <- c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix")
choose_cities <- filter(parks, city %in% cities)
choose_cities$city <- factor(choose_cities$city, levels=cities)
choose_cities$park_pct_city_data <- as.numeric(gsub('.{1}$', '', choose_cities$park_pct_city_data))

#stream plot
p1 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data/5, fill=city, group=city)) +
  geom_stream(color = NA, bw = 0.9, type="ridge") +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="Parkland as percentage\nof city area (%)", title="City parks", 
       subtitle="The percentage of the city area dedicated to parks in the five largest cities has\nbeen stable since 2012, with New York having the highest percentage.") +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=14, hjust = 0, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p1


#facet plot
p2 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_area(aes(group=city), color=NA) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  facet_grid(~city) +
  scale_x_continuous(breaks=c(2014, 2020)) +
  labs(x="", y="Parkland as percentage\nof city area (%)", subtitle="", caption="N.Rennie | Data: The Trust for Public Land") +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=14, hjust = 0, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 1, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.text = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())
p2


#join plots
dev.new(width=8,height=6.5,unit="in", noRStudioGD = TRUE)
p <-  p1 + p2 + plot_layout(ncol = 1) &
  theme(panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        plot.background = element_rect(fill = "ivory2", colour="ivory2"))
p


############################################ V2 ##############################################################
#stream plot
p1 <- ggplot(parks, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = 0.9, type="ridge", extra_span = 0.1) +
  labs(x="", y="Parkland as percentage\nof city area (%)", title="City parks", 
       subtitle="The percentage of the city area dedicated to parks across the US has generally\nincreased between 2012 and 2016. Since 2016, it has remained fairly stable.") +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=14, hjust = 0, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p1

dev.new(width=8,height=6.5,unit="in", noRStudioGD = TRUE)
p <-  p1 + p2 + plot_layout(ncol = 1) &
  theme(panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        plot.background = element_rect(fill = "ivory2", colour="ivory2"))
p
