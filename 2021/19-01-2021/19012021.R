library(tidyverse)
library(maptools)
library(raster)
library(plyr)
library(ggplot2)
library(rgdal)
library(rKenyaCensus)
library(remotes)
#get data
remotes::install_github("Shelmith-Kariuki/rKenyaCensus")
education <- rKenyaCensus::V4_T2.3
county <- rKenyaCensus::V2_T1.2
#proportion university educated 
education$edutotal <- education[,8]/education$Total
#group by gender
d <- tibble(
  SubCounty = education$SubCounty[which(education$Gender == "Male")],
  Male = education$edutotal[which(education$Gender == "Male")],
  Female = education$edutotal[which(education$Gender == "Female")],
  Difference = Male - Female
)
#get a county for each subcounty
d1 <- d[which(d$SubCounty %in% county$SubCounty),]
d1$County <- sapply(d1$SubCounty, function(x) county$County[which(county$SubCounty == x)])
#aggregate by county
d2 <- aggregate(x = d1$Difference, by = list(d1$County), FUN = mean)

#plot
Kenya1<-getData("GADM", country="KE", level=1)
Kenya1_UTM<-spTransform(Kenya1, CRS("+init=EPSG:32737"))  
NAME_1<-Kenya1_UTM@data$NAME_1
count_df<-data.frame(NAME_1, difference=d2[,2]*100)
Kenya1_UTM@data$id <- rownames(Kenya1_UTM@data)
Kenya1_UTM@data <- join(Kenya1_UTM@data, count_df, by="NAME_1")
Kenya1_df <- fortify(Kenya1_UTM)
Kenya1_df <- join(Kenya1_df,Kenya1_UTM@data, by="id")

p <- ggplot() +
  geom_polygon(data = Kenya1_df, aes(x = long, y = lat, group = group, fill = difference), color = "black", size = 0.25) +
  scale_fill_gradient2(
    name="",
    low = "#d8b365",
    mid = "#f5f5f5",
    high = "#366c67",
    midpoint = 0,
    limits = c(-1.5,1.5),
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  xlab("") + ylab("") + 
  labs(title = "University education in Kenya", 
       subtitle = "The colour represents the difference in the percentage of males and females who are \neducated to university level.") +
  theme(aspect.ratio=1, 
        panel.background = element_rect(fill = "#DCDCDC"),
        plot.background = element_rect(fill = "#DCDCDC"),
        legend.background = element_rect(fill = "#DCDCDC"),
        plot.title = element_text(colour = "#366c67", size=18, face="bold"),
        plot.subtitle = element_text(colour = "#366c67", size=12),
        legend.title = element_text(colour = "#366c67", size=12),
        axis.title.x= element_text(colour="#366c67", size=12),
        axis.title.y= element_text(colour="#366c67", size=11),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        )
p
ggsave(p, filename = "19012021.jpg", height=7.27, width=7.39)
