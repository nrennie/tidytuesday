library(tidyverse)
library(tigris)
library(extrafont)
library(patchwork)

#dev.new(width=6,height=6.9,unit="in", noRStudioGD = TRUE)

tuesdata <- tidytuesdayR::tt_load('2021-05-11')

broadband <- tuesdata$broadband
broadband_zip <- tuesdata$broadband_zip

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#check avg broadband availability by state
state_broadband <- broadband %>% group_by(ST)
state_broadband <- state_broadband %>% summarise(avg = mean(as.numeric(`BROADBAND AVAILABILITY PER FCC`), na.rm=T))
max_state <- state_broadband$ST[which.max(state_broadband$avg)]
min_state <- state_broadband$ST[which.min(state_broadband$avg)]

#plot minimum state availability by county 
CT_data <- filter(broadband, ST == "CT")
CT_data$COUNTYFP <- substrRight(as.character(CT_data$`COUNTY ID`), 3)
tracts_max <- tracts(state = 'CT')
ggtract_CT<-fortify(tracts_max, region = "GEOID") 
ggtract_CT<-left_join(ggtract_CT, CT_data, by=c("COUNTYFP")) 


AR_data <- filter(broadband, ST == "AR")
AR_data$COUNTYFP <- substrRight(as.character(AR_data$`COUNTY ID`), 3)
tracts_min <- tracts(state = 'AR')
ggtract_AR<-fortify(tracts_min, region = "GEOID") 
ggtract_AR<-left_join(ggtract_AR, AR_data, by=c("COUNTYFP")) 

#plot for most availability
p1 <- ggplot() +
  geom_sf(data = ggtract_CT, aes(fill=as.numeric(`BROADBAND USAGE`)), colour = "#061541") +
  scale_fill_gradient("% of people with access\nto broadband", low="#f0f9e8" ,high="#061541", limits=c(0,1), 
                      breaks=c(0,1), labels=c("0", "100%"), guide = guide_colourbar(title.position = "top")) +
  annotate("text", x=-72.68202, y=42.0245103+0.5, label="CONNECTICUT", colour="#a8ddb5", fontface="bold", family="Garamond", size=7, hjust=0.5) +
  theme(panel.background = element_rect(fill = "#061541"),
        plot.background = element_rect(fill = "#061541"),
        legend.background = element_rect(fill = "#061541"),
        plot.title = element_text(colour = "#a8ddb5", size=20, face="bold", hjust = 0.5, family="Garamond"),
        plot.subtitle = element_text(colour = "#a8ddb5", size=10, face="bold", hjust = 0.5, family="Garamond"),
        plot.caption = element_text(colour = "#a8ddb5", size=10, face="bold", hjust = 0.5, family="Garamond"),
        legend.title = element_text(colour = "#a8ddb5", size=13, face="bold", hjust = 0.5, family="Garamond"),
        legend.position="bottom",
        plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "#a8ddb5", fill="#060405"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#a8ddb5", size=12, family="Garamond", hjust = 0.5),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p1


#plot for least availability
p2 <- ggplot() +
  geom_sf(data = ggtract_AR, aes(fill=as.numeric(`BROADBAND USAGE`)), colour = "#061541") +
  scale_fill_gradient("% of people with access\nto broadband", low="#f0f9e8" ,high="#061541", limits=c(0,1), 
                      breaks=c(0,1), labels=c("0", "100%"), guide = guide_colourbar(title.position = "top")) +
  annotate("text", x=-92.213, y=36.4862722+0.5, label="ARKANSAS", colour="#a8ddb5", fontface="bold", family="Garamond", size=7, hjust=0.5) +
  theme(panel.background = element_rect(fill = "#061541"),
        plot.background = element_rect(fill = "#061541"),
        legend.background = element_rect(fill = "#061541"),
        plot.title = element_text(colour = "#a8ddb5", size=20, face="bold", hjust = 0.5, family="Garamond"),
        plot.subtitle = element_text(colour = "#a8ddb5", size=10, face="bold", hjust = 0.5, family="Garamond"),
        plot.caption = element_text(colour = "#a8ddb5", size=10, face="bold", hjust = 0.5, family="Garamond"),
        legend.title = element_text(colour = "#a8ddb5", size=13, face="bold", hjust = 0.5, family="Garamond"),
        legend.position="bottom",
        plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "#a8ddb5", fill="#060405"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#a8ddb5", size=12, family="Garamond", hjust = 0.5),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p2

#join plots together
p <- p1 + p2 + plot_layout(ncol = 2)  +
  plot_annotation(
    title = "THE INTERNET INEQUALITY\n",
    subtitle = "There is a large discrepancy in the percentage of people per\ncounty with access to fixed terrestrial broadband at speeds\nof 25 Mbps/3 Mbps.\n\n On average, 98.9% of people in Connecticut have access\nto broadband. In contrast, only 56.8%\nof people living in Arkansas can say the same.", 
    caption = 'N. Rennie | Data: The Verge & Microsoft'
  )  &
  theme(panel.background = element_rect(fill = "#061541", colour="#061541"),
        plot.background = element_rect(fill = "#061541", colour="#061541"),
        plot.title = element_text(colour = "#a8ddb5", size=20, face="bold", hjust = 0.5, family="Garamond"),
        plot.subtitle = element_text(colour = "#a8ddb5", size=14, hjust = 0.5, family="Garamond"),
        plot.caption = element_text(colour = "#a8ddb5", size=10, face="bold", hjust = 0, family="Garamond"))
p

