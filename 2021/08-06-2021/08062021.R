library(tidyverse)
library(extrafont)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load('2021-06-08')
fishing <- tuesdata$fishing

select_fish <- c("Lake Trout", "Blue Pike", "Lake Whitefish")
fish <- filter(fishing, species %in% select_fish)[,c(1,2,3,4)]
f1 <- fish %>% group_by(lake, year, species) %>% summarise(count = sum(grand_total, na.rm=T))

p <- ggplot() +
  geom_tile(data=f1, mapping=aes(x=year, y=lake, fill=count, height=.9)) +
  labs(title="Great Lakes Fishing", caption="N. Rennie | Data: Great Lakes Fishery Commission", y="", x="", 
       subtitle = "The number of different species of fish found in each of the Great Lakes between 1867 and 2015. Lake Whitefish have been found in\nall of the Great Lakes. The Blue Pike was once one of the most populous species, but has since become extinct.") +
  scale_fill_gradient("Number of fish", low="#5e5a32", high="#f1f1e2", na.value="#e8ddb7", limits=c(0,160000), breaks=c(0,160000), labels=c("0", "160,000")) +
  facet_wrap(~species, ncol=1) +
  scale_x_continuous(limits=c(1867,2015), breaks=seq(1870,2015,30)) +
  guides(fill = guide_colorbar(title.position = "bottom")) +
  theme_gray() +
  theme(strip.background =element_rect(fill=alpha("#e0d7b0", 0.5)),
        strip.text = element_text(colour = '#7c683b', family="Juice ITC", size=16, face=2),
        plot.background = element_rect(fill = "#e0d7b0", colour="#e0d7b0"),
        panel.background = element_rect(fill = "#e0d7b0", colour="#e0d7b0"),
        legend.background = element_rect(fill = "#e0d7b0"),
        legend.key = element_rect(fill = "#e0d7b0", colour="#e0d7b0"), 
        legend.text =  element_text(colour = "#7c683b", size=12, family="Juice ITC"),
        legend.title =  element_text(colour = "#7c683b", size=12, family="Juice ITC", hjust=0.5),
        plot.title = element_text(colour = "#7c683b", size=22, face="bold", hjust = 0, family="Juice ITC"),
        plot.subtitle = element_text(colour = "#7c683b", size=16, hjust = 0, family="Juice ITC"),
        plot.caption = element_text(colour = "#7c683b", size=12, hjust = 1.5, family="Juice ITC"),
        legend.position="bottom",
        plot.margin = unit(c(0.3, 8, 0.3, 0.8), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_text(colour = "#7c683b", size=12, family="Juice ITC"),
        panel.grid.major = element_line(colour="#7c683b"),
        panel.grid.minor = element_blank())

p

#dev.new(width=13,height=8,unit="cm", noRStudioGD = TRUE)

fish1 <- "C:/Users/rennien/OneDrive - Lancaster University/Programming/git/tidytuesday/2021/08-06-2021/blue_pike.png"
fish2 <- "C:/Users/rennien/OneDrive - Lancaster University/Programming/git/tidytuesday/2021/08-06-2021/lake_trout.png"
fish3 <- "C:/Users/rennien/OneDrive - Lancaster University/Programming/git/tidytuesday/2021/08-06-2021/lake_whitefish.png"

q <- ggdraw() + 
  draw_plot(p) +
  draw_image(
    fish1, x = 0.87, y = 0.85, hjust = 0.5, vjust = 1, halign = 1, valign = 1,
    width = 0.18
  ) +
  draw_image(
    fish2, x = 0.87, y = 0.6, hjust = 0.5, vjust = 1, halign = 1, valign = 1,
    width = 0.18
  ) +
  draw_image(
    fish3, x = 0.87, y = 0.35, hjust = 0.5, vjust = 1, halign = 1, valign = 1,
    width = 0.18
  ) +
  draw_label(label="Blue Pike\n(Sander vitreus glaucus)", x=0.87, y=0.67, hjust=0.5, fontfamily="Juice ITC", size=14, colour = "#7c683b") +
  draw_label(label="Lake Trout\n(Salvelinus namaycush)", x=0.87, y=0.42, hjust=0.5, fontfamily="Juice ITC", size=14, colour = "#7c683b") +
  draw_label(label="Lake Whitefish\n(Coregonus clupeaformis)", x=0.87, y=0.17, hjust=0.5, fontfamily="Juice ITC", size=14, colour = "#7c683b")

q


