library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(extrafont)
library(patchwork)
tuesdata <- tidytuesdayR::tt_load('2021-05-04')
water <- tuesdata$water

tech_change <- function(input){
  if (input %in% unique(water$water_tech)[c(2,5,7,8,9,12,14,15,16,17,18,19)]){
    return("Hand Pump")
  } 
  if (input %in% unique(water$water_tech)[c(3,11,20)]) {
    return("Mechanized Pump")
  } 
  else{
    return("Other")
  }
}

countries <- c("Kenya","Ethiopia", "Zimbabwe", "Namibia")
world <- ne_countries(scale = "medium", returnclass = "sf")

#Kenya
kenya <- world[world$name == "Kenya",] 
ken_data <- filter(water, country_name == "Kenya")
ken_data <- ken_data[which(!is.na(ken_data$water_tech)),]
ken_data$tech <- sapply(ken_data$water_tech, function(x) tech_change(x))

ken_dat <- data.frame(Longitude = ken_data$lon_deg, Latitude = ken_data$lat_deg)
coordinates(ken_dat) <- ~ Longitude + Latitude
proj4string(ken_dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
ken_pnts_sf <- st_as_sf(ken_dat, coords = c('Latitude', 'Longitude'), crs = st_crs(map))
ken_pts <- ken_data[st_intersects(ken_pnts_sf, kenya, sparse=F)[,1],]


p1 <- ggplot() +
  geom_sf(data = kenya, fill="#fba55a") +
  coord_sf(xlim = c(30.5, 45.5), ylim = c(-6,6)) +
  geom_point(data=ken_pts, aes(x=lon_deg, y=lat_deg, colour=tech), size=1.5) +
  scale_colour_manual("Each point shows the location of a water source,\nwith the colour representing the method of water\ntransportation e.g. hand pump.\n ", values=c("Hand Pump"="red1", "Mechanized Pump"="purple2", "Other"="green4"), breaks=c("Hand Pump", "Mechanized Pump", "Other"), labels=c("Hand Pump      ", "Mechanized Pump     ", "Other")) +
  labs(title="\nKENYA") +
  guides(col=guide_legend(ncol=3, title.position = "top")) +
  theme(panel.background = element_rect(fill = "#060405"),
        plot.background = element_rect(fill = "#060405"),
        legend.background = element_rect(fill = "#060405"),
        plot.title = element_text(colour = "#fba55a", size=20, face="bold", hjust = 0.5, family="Elephant"),
        plot.subtitle = element_text(colour = "#fba55a", size=10, face="bold", hjust = 0.5, family="Elephant"),
        plot.caption = element_text(colour = "#fba55a", size=10, face="bold", hjust = 0.5, family="Elephant"),
        legend.title = element_text(colour = "#d0605e", size=13, face="bold", hjust = 0.5, family="Elephant"),
        legend.position="bottom",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "#060405", fill="#060405"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#fba55a", size=12, family="Elephant"),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p1


##########################################################################################################################
#########################################################################################################################

#Ethiopia
ethiopia <- world[world$name == "Ethiopia",] 
eth_data <- filter(water, country_name == "Ethiopia")
eth_data <- eth_data[which(!is.na(eth_data$water_tech)),]
eth_data$tech <- sapply(eth_data$water_tech, function(x) tech_change(x))

eth_dat <- data.frame(Longitude = eth_data$lon_deg, Latitude = eth_data$lat_deg)
coordinates(eth_dat) <- ~ Longitude + Latitude
proj4string(eth_dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
eth_pnts_sf <- st_as_sf(eth_dat, coords = c('Latitude', 'Longitude'), crs = st_crs(map))
eth_pts <- eth_data[st_intersects(eth_pnts_sf, ethiopia, sparse=F)[,1],]

p2 <- ggplot() +
  geom_sf(data = ethiopia, fill="#fba55a") +
  coord_sf(xlim = c(33, 48), ylim = c(3,15)) +
  geom_point(data=eth_pts, aes(x=lon_deg, y=lat_deg, colour=tech), size=1.5) +
  scale_colour_manual("", values=c("Hand Pump"="red1", "Mechanized Pump"="purple2", "Other"="green4"), breaks=c("Hand Pump", "Mechanized Pump", "Other"), labels=c("Hand Pump    ", "Mechanized Pump ", "Other")) +
  labs(title="\nETHIOPIA") +
  theme(panel.background = element_rect(fill = "#060405"),
        plot.background = element_rect(fill = "#060405"),
        legend.background = element_rect(fill = "#060405"),
        plot.title = element_text(colour = "#fba55a", size=20, face="bold", hjust = 0.5, family="Elephant"),
        plot.subtitle = element_text(colour = "#fba55a", size=10, face="bold", hjust = 0.5, family="Elephant"),
        plot.caption = element_text(colour = "#fba55a", size=10, face="bold", hjust = 0.5, family="Elephant"),
        legend.title = element_blank(),
        legend.position="bottom",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "#060405", fill="#060405"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#fba55a", size=12, family="Elephant"),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p2


##########################################################################################################################
#########################################################################################################################

#Zimbabwe
zimbabwe <- world[world$name == "Zimbabwe",] 
zim_data <- filter(water, country_name == "Zimbabwe")
zim_data <- zim_data[which(!is.na(zim_data$water_tech)),]
zim_data$tech <- sapply(zim_data$water_tech, function(x) tech_change(x))

zim_dat <- data.frame(Longitude = zim_data$lon_deg, Latitude = zim_data$lat_deg)
coordinates(zim_dat) <- ~ Longitude + Latitude
proj4string(zim_dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
zim_pnts_sf <- st_as_sf(zim_dat, coords = c('Latitude', 'Longitude'), crs = st_crs(map))
zim_pts <- zim_data[st_intersects(zim_pnts_sf, zimbabwe, sparse=F)[,1],]

p3 <- ggplot() +
  geom_sf(data = zimbabwe, fill="#fba55a") +
  coord_sf(xlim = c(33, 48)-11, ylim = c(3,15)-28) +
  geom_point(data=zim_pts, aes(x=lon_deg, y=lat_deg, colour=tech), size=1) +
  scale_colour_manual("", values=c("Hand Pump"="red1", "Mechanized Pump"="purple2", "Other"="green4"), breaks=c("Hand Pump", "Mechanized Pump", "Other"), labels=c("Hand Pump    ", "Mechanized Pump ", "Other")) +
  labs(title="\nZIMBABWE") +
  theme(panel.background = element_rect(fill = "#060405"),
        plot.background = element_rect(fill = "#060405"),
        legend.background = element_rect(fill = "#060405"),
        plot.title = element_text(colour = "#fba55a", size=20, face="bold", hjust = 0.5, family="Elephant"),
        plot.subtitle = element_text(colour = "#fba55a", size=10, face="bold", hjust = 0.5, family="Elephant"),
        plot.caption = element_text(colour = "#fba55a", size=10, face="bold", hjust = 0.5, family="Elephant"),
        legend.title = element_blank(),
        legend.position="bottom",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "#060405", fill="#060405"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#fba55a", size=12, family="Elephant"),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p3

##########################################################################################################################
#########################################################################################################################

#Namibia
namibia <- world[world$name == "Namibia",] 
nam_data <- filter(water, country_name == "Namibia")
nam_data <- nam_data[which(!is.na(nam_data$water_tech)),]
nam_data$tech <- sapply(nam_data$water_tech, function(x) tech_change(x))

nam_dat <- data.frame(Longitude = nam_data$lon_deg, Latitude = nam_data$lat_deg)
coordinates(nam_dat) <- ~ Longitude + Latitude
proj4string(nam_dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
nam_pnts_sf <- st_as_sf(nam_dat, coords = c('Latitude', 'Longitude'), crs = st_crs(map))
nam_pts <- nam_data[st_intersects(nam_pnts_sf, namibia, sparse=F)[,1],]

p4 <- ggplot() +
  geom_sf(data = namibia, fill="#fba55a") +
  coord_sf(xlim = c(33, 48)-22, ylim = c(3,17)-33) +
  geom_point(data=nam_pts, aes(x=lon_deg, y=lat_deg, colour=tech), size=1.5) +
  scale_colour_manual("", values=c("Hand Pump"="red1", "Mechanized Pump"="purple2", "Other"="green4"), breaks=c("Hand Pump", "Mechanized Pump", "Other")) +
  labs(title="\nNAMIBIA") +
  theme(panel.background = element_rect(fill = "#060405"),
        plot.background = element_rect(fill = "#060405"),
        legend.background = element_rect(fill = "#060405"),
        plot.title = element_text(colour = "#fba55a", size=20, face="bold", hjust = 0.5, family="Elephant"),
        plot.subtitle = element_text(colour = "#fba55a", size=10, face="bold", hjust = 0.5, family="Elephant"),
        plot.caption = element_text(colour = "#fba55a", size=10, face="bold", hjust = 0.5, family="Elephant"),
        legend.title = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "#060405", fill="#060405"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#fba55a", size=12, family="Elephant"),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p4

#join plots
leg <- get_legend(p1)
p <- p1 + p2 + p3 + p4 + plot_spacer() + plot_spacer() + plot_layout(ncol = 3, guides = 'collect')  +
  plot_annotation(
    subtitle = 'WATER ACCESS POINTS',
    caption = 'N. Rennie | Data: Water Point Data Exchange'
  ) +
  inset_element(leg, left = -0.2, bottom = 0.8, right = 0, top = 0) &
  theme(panel.background = element_rect(fill = "#060405", colour="#060405"),
        plot.background = element_rect(fill = "#060405", colour="#060405"),
        legend.position = "none",
        plot.subtitle = element_text(colour = "#d0605e", size=22, face="bold", hjust = 0.86, vjust=-60, family="Elephant"),
        plot.caption = element_text(colour = "#fba55a", size=10, face="bold", hjust = 0, family="Elephant"))
p

