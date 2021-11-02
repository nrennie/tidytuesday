library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge) 
library(tmap)    
library(leaflet)  
library(ggplot2)
library(maptools)
library(sp)

#function from rpubs.com/walkerke/points_to_line
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

#Create points data other places called Glasgow
d <- tibble(lat=c(33.6110306, 34.9837379, 41.5526085, 39.6023750, 30.7224197, 40.9405670, 41.0066338, 
                  39.5495186, 36.9958382, 44.3449745, 39.2279015, 38.7541120, 48.1953468, 40.3086815, 
                  43.4338722, 40.6450043, 37.630909, 38.2125664, 5.45247, 43.9944382, -43.9858284),
            lon=c(-86.9521408, -115.8711166, -71.8855438, -75.7441101, -83.8754425, -91.780365, -91.9650497, 
                  -90.480217, -85.9118652, -92.1201172, -92.8415756, -90.1943207, -106.6376266, -81.5506744, 
                  -124.2146378, -80.5079575, -79.4483414, -81.4228439, -55.2431, -79.2041702, 170.4809723))
xy <- d[,c(2, 1)]
spdf <- SpatialPointsDataFrame(coords = xy, data = d,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#Create points df for Glasgow, Scotland
d1 <- tibble(lat=c(55.864239),
            lon=c(-4.251806))
xy1 <- d1[,c(2, 1)]
spdf1 <- SpatialPointsDataFrame(coords = xy1, data = d1,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#create lines object
df <- rbind(d1 %>% slice(rep(1:n(), each = nrow(d))), d)
dat <- df[unlist(lapply(1:21, function(i) c(i, i+21))), ]
id_field = rep(1:nrow(d), each=2)
ptl_df <- cbind(dat, id_field)
ptl <- points_to_line(data=ptl_df, long="lon", lat="lat", id_field = "id_field", sort_field = NULL)

tmap_style("classic")
#plot map
p <- 
  #plot world map
  tm_shape(world) +
  tm_fill() +
  tm_borders() + 
  #plot lines
  tm_shape(ptl) +
  tm_lines(col="black") +
  #plot other glasgows
  tm_shape(spdf)+
  tm_symbols(size=0.3, col="#cb9d06") +
  #plot glasgow
  tm_shape(spdf1)+
  tm_symbols(size=0.5, col="#c90055") +
  #make it pretty
  tm_layout(legend.position = c("right", "top"), 
            title= 'Glasgow Goes Global',  
            title.position = c('left', 'top'), 
            inner.margins = c(.1, .02, .15, .02)) +
  tm_credits("There are at least 21 other places\nwith Glasgow in the name around\nthe globe, including the Glasgow\nRange of mountains in New Zealand.\n\n\n\n\n\nN. Rennie | Data: spDataLarge & Wikipedia", 
             position = c('left', 'bottom'))
p

#save
tmap_save(p, filename = "02112021.png")

