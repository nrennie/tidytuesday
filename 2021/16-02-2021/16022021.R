library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-02-16')

freed_slaves <- tuesdata$freed_slaves

#challenge 4

d <- data.frame(year=freed_slaves$Year, slaves=freed_slaves$Slave, free=freed_slaves$Free, label=sapply(freed_slaves$Free, function(x) paste(x, "%", sep="")))
p <- ggplot(data = d, aes(year)) + 
  geom_ribbon(aes(ymin = rep(0,length(year)), ymax = slaves), fill="black") +
  geom_ribbon(aes(ymin = slaves, ymax = rep(100,length(year))), fill = "springgreen4") +
  geom_segment(aes(x=year, xend = year, y=0, yend = 100), colour=alpha("black", 0.2)) +
  ylim(0,105) + 
  labs(title="PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES .\n\nPROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÉRIQUE .\n", subtitle="DONE BY ATLANTA UNIVERSITY\n") +
  geom_text(aes(x=year, y=c(slaves[1:length(slaves)-1],slaves[length(slaves)-1])+3, label=label), colour="black", size=4, fontface=2) +
  geom_text(aes(x=year, y=103, label=year), colour="black", fontface=2, size=5) +
  annotate("text", x = 1830, y = 50, label = "SLAVES\nESCLAVES", colour = "wheat2", size=5, fontface=2) +
  annotate("text", x = 1830, y = 96, label = "FREE - LIBRE", colour = "black", size=5, fontface=2) +
  theme(panel.background = element_rect(fill = "wheat2"),
        plot.background = element_rect(fill = "wheat2"),
        legend.background = element_rect(fill = "wheat2"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size=10, face="bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.key = element_rect(size = 1.2, colour = "#c51b8a"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#c51b8a", size=12),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p
ggsave(p, filename = "16022021.jpg")





