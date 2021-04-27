library(tidyverse)
library(viridis)

tuesdata <- tidytuesdayR::tt_load('2021-04-27')
departures <- tuesdata$departures

#count by fiscal year, and departure code
dep_data <- select(departures, fyear, departure_code)
dep_data$count <- 1
dep_data <- drop_na(dep_data)
dep_data <- dep_data %>% group_by(fyear, departure_code)
dep_data <- dep_data %>% summarise(count = sum(count))
dep_data <- filter(dep_data, departure_code %in% c(1:7))
dep_data <- filter(dep_data, fyear <= 2018)

#plot stacked area
p <- ggplot(dep_data, aes(x=fyear, y=count, fill=factor(departure_code))) + 
  geom_area(alpha=0.5) +
  labs(title="CEO Departures", 
       subtitle="This data looks at the reasons for a CEOs departure in S&P 1500 firms. The majority\nof CEOs leave their company for voluntary reasons. The most common reason is\nretirement.", 
       caption="\nN. Rennie | Data: DataIsPlural",
       y="Number of CEO Departures\n") +
  geom_errorbar(mapping=aes(x=2019, ymin=80, ymax=215), width=0.5, size=0.5, color="#016c59") + 
  geom_errorbar(mapping=aes(x=2019, ymin=221, ymax=288), width=0.5, size=0.5, color="#016c59") + 
  
  scale_fill_viridis(discrete = T, labels = c("Death", "Illness", "Dismissed for\njob performance", 
                                              "Dismissed for\nlegal concerns", "Retired", "New opportunity",
                                              "Other")) +
  xlim(1987, 2025) +
  annotate("text", x=2023, y=260, label="Involuntary", colour = "#016c59", size=3, hjust=0.5, family="Palatino Linotype") +
  annotate("text", x=2023, y=150, label="Voluntary", colour = "#016c59", size=3, hjust=0.5, family="Palatino Linotype") +
  guides(fill=guide_legend(ncol=1)) +
  theme(axis.text=element_text(colour = "#1c9099", size=10, hjust=0, family="Palatino Linotype"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "#1c9099", size=10, hjust=0.5, family="Palatino Linotype"),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#ece2f0", color = NA),
        panel.background = element_rect(fill = "#ece2f0"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position="right",
        plot.title = element_text(colour = "#016c59", size=15, face="bold", hjust=0, family="Bernard MT Condensed"),
        plot.subtitle = element_text(colour = "#016c59", size=10, hjust=0, family="Palatino Linotype"),
        plot.caption = element_text(colour = "#016c59", size = 8, hjust=0, family="Palatino Linotype"),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.height=unit(0.6,"cm"),
        legend.key.width=unit(0.6,"cm"),
        legend.spacing.y = unit(1,"cm"),
        legend.text = element_text(colour = "#016c59", size=8, hjust=0), 
        legend.title=element_blank())
p

dev.new(width=6.5,height=4,unit="in", noRStudioGD = TRUE)
ggsave(p, filename = "27042021.jpg", bg = "transparent", height=4, width=6.5, unit="in")



