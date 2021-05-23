#Life expectancy for people aged 0-1 years in 2017-2019, make and female
library(tidyverse)
library(sf)
library(ggplot2)
library(patchwork)
library(extrafont)

life <- read.csv("data.csv")

male_life <- filter(life, sex == "male")
male_life_df <- male_life[which(male_life$Geography != "England"),]
male_life_df$difference <- male_life_df$v4_2 - male_life$v4_2[which(male_life$Geography == "England")]
male_life_df <- male_life_df[order(male_life_df$administrative.geography),]

female_life <- filter(life, sex == "female")
female_life_df <- female_life[which(female_life$Geography != "England"),]
female_life_df$difference <- female_life_df$v4_2 - female_life$v4_2[which(female_life$Geography == "England")]
female_life_df <- female_life_df[order(female_life_df$administrative.geography),]

shp = st_read("Regions_(December_2019)_Boundaries_EN_BFC.shp")

p1 <- ggplot(data = shp) +
  geom_sf(aes(fill=male_life_df$difference), size=0.3) + 
  geom_text(data=data.frame(x=550000, y=600000, label="MALE"), aes(x=x,y=y,label=label), colour="#276419", hjust=0.5, size=9, fontface="bold") +
  scale_fill_gradient2("", low="#4d9221", high="#c51b7d", mid="#f7f7f7", limits=c(-2,2), breaks=c(-2,0,2), labels=c("-2 Years", "0", "2 Years")) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(colour = "#276419", size=18, face="bold"),
        plot.subtitle = element_text(colour = "#276419", size=12),
        legend.title = element_blank(),
        legend.text = element_text(size=11),
        legend.key.size = unit(0.7, "cm"),
        legend.position=c(0.1,0.4),
        legend.justification=c(0,0),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p1

p2 <- ggplot(data = shp) +
  geom_sf(aes(fill=female_life_df$difference), size=0.3) + 
  geom_text(data=data.frame(x=550000, y=600000, label="FEMALE"), aes(x=x,y=y,label=label), colour="#276419", hjust=0.5, size=9, fontface="bold") +
  scale_fill_gradient2("", low="#4d9221", high="#c51b7d", mid="#f7f7f7", limits=c(-2,2), breaks=c(-2,0,2), labels=c("-2 Years", "0", "2 Years")) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(colour = "#276419", size=18, face="bold"),
        plot.subtitle = element_text(colour = "#276419", size=12),
        legend.title = element_blank(),
        legend.text = element_text(size=11),
        legend.key.size = unit(0.7, "cm"),
        legend.position=c(0.1,0.4),
        legend.justification=c(0,0),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p2


p <- p1 + p2 + plot_layout(ncol = 2)  +
  plot_annotation(
    title = 'LIFE EXPECTANCY IN ENGLAND',
    subtitle = "Difference in life expectancy from the national average for children\naged 0-1 years old in 2017-2019. Life expectancy for men and\nwomen in England is lowest in the North East.",
    caption = 'N. Rennie | Data: Office for National Statistics'
  ) &
  theme(panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.background = element_rect(fill = "gray95", colour="gray95"),
        plot.title = element_text(colour = "#276419", size=22, face="bold", hjust = 0.5, family="Arial"),
        plot.subtitle = element_text(colour = "#276419", size=14, hjust = 0.5, family="Arial"),
        plot.caption = element_text(colour = "#276419", size=10, face="bold", hjust = 0, family="Arial"))
p


