library(tidytuesdayR)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#load data
tuesdata <- tidytuesdayR::tt_load('2021-01-12')
artwork <- tuesdata$artwork
artists <- tuesdata$artists

#list of common artistIDs across artwork and artists
artistID <- intersect(artists$id, artwork$artistId)

artwork_artistID <- artwork$artistId[which(artwork$artistId %in% artistID)]
artists_artistID <- artists$id[which(artists$id %in% artistID)]

#find birthplaces of artists
birthplace <- (artists$placeOfBirth[which(artists$id %in% artistID)])[order(artists_artistID)]

#number of artworks by each artist 
num_artworks <- table(artwork_artistID)
num_artworks_by_artist <- num_artworks[order(as.numeric(names(num_artworks)))] 

#join together and remove NA values
d <- na.omit(data.frame(birthplace, num_artworks_by_artist))

#get coordinates of birthplaces
#write.table(d$birthplace, "birthplace.txt", row.names=FALSE, sep=",", col.names=FALSE)
#use https://www.gpsvisualizer.com/geocoder/ to obtain latitude and longitude 
coord <- read.table("coordinates.txt", row.names=NULL) 
lat <- as.numeric(coord[,1])
lon <- as.numeric(coord[,2])

#get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

#make plot
points_df <-  na.omit(data.frame(longitude = lon, latitude = lat, num_artworks_by_artist = d$Freq, colsize = as.character(pmin(6, d$Freq))))
p <- ggplot(data = world) +
  geom_sf(fill= "antiquewhite") +
  geom_point(data = points_df, aes(x = longitude, y = latitude, size = colsize, fill = colsize), shape = 21) +
  scale_size_manual(name="Number \nof Artworks", values=c(1,2,3,4,5,6), labels=c("1","2","3","4","5",">5" )) +
  scale_fill_manual(name="Number \nof Artworks", values=c("6"="#B4D6C1","5"="#8DC3A7","4"="#6BAF92","3"="#4E9C81","2"="#358873","1"="#207567"), labels=c("1","2","3","4","5",">5" )) +
  xlab("Longitude") + ylab("Latitude") + 
  labs(title = "Birthplaces of Artists", 
       subtitle = "Each point represents the birthplace of an artist whose work is owned by Tate, with the size and colour \nof the point representing the number of their artworks that are owned by Tate.") +
  theme(panel.background = element_rect(fill = "aliceblue"),
        plot.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(colour = "#207567", size=18, face="bold"),
        plot.subtitle = element_text(colour = "#207567", size=12),
        legend.title = element_text(colour = "#207567", size=12),
        axis.title.x= element_text(colour="#207567", size=12),
        axis.title.y= element_text(colour="#207567", size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p
ggsave(p, filename = "12012021.jpg", units="in")


