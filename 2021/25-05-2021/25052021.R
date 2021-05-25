library(tidyverse)
library(lubridate)
library(extrafont)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load('2021-05-25')
records <- tuesdata$records
drivers <- tuesdata$drivers

records_data <- filter(records, type == "Single Lap" & shortcut == "No")
drivers_nation <- unique(drivers[,c(2,6)])
records_nation <- left_join(records_data, drivers_nation, by="player")
records_nation$count <- 1
records_nation <- filter(records_nation, !is.na(nation))
r <- records_nation %>% group_by(track, nation) %>% summarise(all_records = sum(count))

#heat map of tracks and number of records held in each country
p1 <- ggplot(data=r, aes(x=nation, y=track, fill=all_records)) + 
  geom_tile() +
  scale_fill_gradient("Number of\nWorld Records\n", low="#feb24c", high="#800026", na.value = "#ffffcc", limits=c(1,20), breaks=c(1,10,20)) +
  labs(subtitle="The number of world records for a single lap for each\nof the tracks for Mario Kart 64, for players in different\ncountries. Only Australia, Germany, the Netherlands,\nand the USA have held world records on every track.\n") +
  theme(panel.background = element_rect(fill = "#ffffcc"),
        plot.background = element_rect(fill = "#ffffcc"),
        legend.background = element_rect(fill = "#ffffcc"),
        plot.title = element_text(colour = "#800026", size=20, face="bold", hjust = 0, family="Modern No. 20"),
        plot.subtitle = element_text(colour = "#800026", size=10, hjust = 0, family="Verdana"),
        legend.position="right",
        plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.spacing.x = unit(0.5,"cm"),
        legend.title = element_text(colour = "#800026", size=10, hjust = 0.5, family="Modern No. 20"),
        legend.text = element_text(colour="#800026", size=10, family="Modern No. 20", hjust = 0.5),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_text(colour = "#800026", size=10, hjust = 0, vjust=0.5, family="Modern No. 20", angle=-90),
        axis.text.y=element_text(colour = "#800026", size=10, hjust = 1, family="Modern No. 20"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )

p1

#######################################################################################################################
#######################################################################################################################

#when did different players hold the record
records_data2 <- filter(records, type == "Single Lap" & shortcut == "No" & track == "Mario Raceway")
records_data2$end_date <- records_data2$date + records_data2$record_duration
r2 <- records_data2 %>% group_by(player) %>% summarise(all_records = min(date))
r2 <- r2[order(r2$all_records),]

p2 <- ggplot(data=records_data2) + 
  geom_point(mapping=aes(x=date, y=player, colour=system_played)) +
  geom_point(mapping=aes(x=end_date, y=player, colour=system_played)) + 
  geom_segment(mapping=aes(x=date, xend=end_date, y=player, yend=player, colour=system_played)) +
  scale_y_discrete(limits = factor(r2$player, levels=r2$player)) +
  scale_colour_manual("System\nplayed", values=c("#2166ac", "#c51b7d")) +
  labs(subtitle="World records for fastest time for a single lap of\nMario Kart 64: Mario Raceway. The average time\nfor a player to hold a world record is 453 days\nfor NTSC systems, and 218 days for PAL systems.\n") +
  theme(panel.background = element_rect(fill = "#ffffcc"),
        plot.background = element_rect(fill = "#ffffcc"),
        legend.background = element_rect(fill = "#ffffcc"),
        plot.title = element_text(colour = "#800026", size=20, face="bold", hjust = 0, family="Modern No. 20"),
        plot.subtitle = element_text(colour = "#800026", size=10, hjust = 0, family="Verdana"),
        legend.position="right",
        legend.key = element_rect(colour = "#ffffcc", fill="#ffffcc"),
        plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.spacing.x = unit(0.5,"cm"),
        legend.title = element_text(colour = "#800026", size=10, hjust = 0.5, family="Modern No. 20"),
        legend.text = element_text(colour="#800026", size=10, family="Modern No. 20", hjust = 0.5),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_text(colour = "#800026", size=10, family="Modern No. 20"),
        axis.text.y=element_text(colour = "#800026", size=10, hjust = 1, family="Modern No. 20"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_line(colour=alpha("#800026",0.3)),
        panel.grid.minor = element_blank()
  )

p2


#dev.new(width=10,height=6,unit="in", noRStudioGD = TRUE)


p <- p1 + p2 + plot_layout(ncol = 2) +
  plot_annotation(
    caption = 'N. Rennie | Data: Mario Kart World Records',
    title = '\nMARIO KART 64 WORLD RECORDS') &
  theme(panel.background = element_rect(fill = "#ffffcc", colour="#ffffcc"),
        plot.background = element_rect(fill = "#ffffcc", colour="#ffffcc"),
        plot.title = element_text(colour = "#800026", size=24, face="bold", hjust = 0.5, family="Modern No. 20"),
        plot.caption = element_text(colour = "#800026", size=10, hjust = 1, family="Modern No. 20"))
p

