library(tidyverse)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load('2021-03-23')
unvotes <- tuesdata$unvotes
roll_calls <- tuesdata$roll_calls
issues <- tuesdata$issues

#look at data
issues_nuc <- filter(issues, issue == "Nuclear weapons and nuclear material")
roll_calls_nuc <- filter(roll_calls, rcid %in% issues_nuc$rcid & importantvote == 1)
d2010 <- filter(unvotes, rcid == 5079)
d2011 <- filter(unvotes, rcid == 5157)
d2013 <- filter(unvotes, rcid == 5270)

library(plyr)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)

### 2010 ###
#join to world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
d2010$iso_a2 <- d2010$country_code
k <- join(data.frame(iso_a2=world$iso_a2), d2010, by = "iso_a2", type = "left")
k$vote[which(is.na(k$vote))] <- "nodata"
world$vote <- k$vote
#plot data
worldmap2010 <- ggplot() +
  geom_sf(data = world, aes(fill = vote)) +
  labs(title="2010") +
  scale_fill_manual("", values = c("yes"="#58A6A6", "no"="#421E22", "abstain"="#EFA355", "nodata"="lightgray"),
                    breaks = c("yes", "no", "abstain", "nodata"),
                    labels=c("yes"="Yes","no"="No","abstain"="Abstain","nodata"="No Data")) +
   theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "lemonchiffon"),
         plot.background = element_rect(fill = "lemonchiffon"),
         plot.title = element_text(size = 20, face=2, colour="#58A6A6", hjust = 0.5),
         legend.background = element_rect(fill = "lemonchiffon"),
         legend.position = "none")
worldmap2010


### 2011 ###
#join to world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
d2011$iso_a2 <- d2011$country_code
k <- join(data.frame(iso_a2=world$iso_a2), d2011, by = "iso_a2", type = "left")
k$vote[which(is.na(k$vote))] <- "nodata"
world$vote <- k$vote
#plot data
worldmap2011 <- ggplot() +
  geom_sf(data = world, aes(fill = vote)) +
  labs(title="2011") +
  scale_fill_manual("", values = c("yes"="#58A6A6", "no"="#421E22", "abstain"="#EFA355", "nodata"="lightgray"),
                    breaks = c("yes", "no", "abstain", "nodata"),
                    labels=c("yes"="Yes","no"="No","abstain"="Abstain","nodata"="No Data")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lemonchiffon"),
        plot.background = element_rect(fill = "lemonchiffon"),
        plot.title = element_text(size = 20, face=2, colour="#58A6A6", hjust = 0.5),
        legend.background = element_rect(fill = "lemonchiffon"),
        legend.position = "none")
worldmap2011


### 2013 ###
#join to world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
d2013$iso_a2 <- d2013$country_code
k <- join(data.frame(iso_a2=world$iso_a2), d2013, by = "iso_a2", type = "left")
k$vote[which(is.na(k$vote))] <- "nodata"
world$vote <- k$vote
#plot data
worldmap2013 <- ggplot() +
  geom_sf(data = world, aes(fill = vote)) +
  labs(title="2013") +
  scale_fill_manual("", values = c("yes"="#58A6A6", "no"="#421E22", "abstain"="#EFA355", "nodata"="lightgray"),
                    breaks = c("yes", "no", "abstain", "nodata"),
                    labels=c("yes"="Yes","no"="No","abstain"="Abstain","nodata"="No Data")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lemonchiffon"),
        plot.background = element_rect(fill = "lemonchiffon"),
        plot.title = element_text(size = 20, face=2, colour="#58A6A6", hjust = 0.5),
        legend.background = element_rect(fill = "lemonchiffon"),
        legend.position = "none")
worldmap2013

dev.new(width=5.5,height=9,unit="in", noRStudioGD = TRUE)
#join plots together
p <- plot_grid(worldmap2010, worldmap2011, worldmap2013, ncol=1, nrow=3)
legend_b <- get_legend(
  worldmap2010 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)
q <- plot_grid(p, legend_b, ncol = 1, rel_heights = c(1, .1)) +
  theme(plot.background = element_rect(fill="lemonchiffon", color = NA), 
        plot.margin = unit(c(2, 0, 1, 0), "cm"), #top, right, bottom, left
        ) +
  draw_label("UN Votes on the Total Eliminaton of Nuclear Weapons", x = 0.5, y = 1.1, hjust = 0.5, vjust = 0.5, 
             color = "#421E22", size = 12, fontface=2) +
  draw_label("N.Rennie | Data: Harvard Dataverse", x = 0.97, y = -0.05, hjust = 1, vjust = 0, 
             color = "#421E22", size = 8) +
  draw_label("There have been multiple votes in the UN on the total elimination\nof nuclear weapons. Voting records for different countries are shown here.", 
             x = 0.5, y = 1.03, hjust = 0.5, vjust = 0, 
             color = "#421E22", size = 10) + 
  draw_label("93%\nYes", x = 0.1, y = 0.8, hjust = 0.5, vjust = 0.5, 
             color = "#58A6A6", size = 12, fontface=2) +
  draw_label("93%\nYes", x = 0.1, y = 0.5, hjust = 0.5, vjust = 0.5, 
             color = "#58A6A6", size = 12, fontface=2) +
  draw_label("92%\nYes", x = 0.1, y = 0.2, hjust = 0.5, vjust = 0.5, 
             color = "#58A6A6", size = 12, fontface=2) +
  draw_label("6%\nAbstain", x = 0.9, y = 0.8, hjust = 0.5, vjust = 0.5, 
             color = "#EFA355", size = 12, fontface=2) +
  draw_label("6%\nAbstain", x = 0.9, y = 0.5, hjust = 0.5, vjust = 0.5, 
             color = "#EFA355", size = 12, fontface=2) +
  draw_label("7%\nAbstain", x = 0.9, y = 0.2, hjust = 0.5, vjust = 0.5, 
             color = "#EFA355", size = 12, fontface=2) 
q

