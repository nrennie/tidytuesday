library(tidyverse)
library(cowplot)
library(extrafont)
library(lubridate)

tuesdata <- tidytuesdayR::tt_load('2021-09-14')

billboard <- tuesdata$billboard
audio_features <- tuesdata$audio_features
audio <- audio_features[,c(1,4,10)]

#function for genre
select_genre <- function(word){
  if (is.na(word)){
    return(NA)
  }
  if (grepl("pop", word, fixed = TRUE)){
    return("Pop")
  }
  if (grepl("rap", word, fixed = TRUE)){
    return("Rap")
  }
  if (grepl("rock", word, fixed = TRUE)){
    return("Rock")
  }
  if (grepl("country", word, fixed = TRUE)){
    return("Country")
  }
  if (grepl("r&b", word, fixed = TRUE)){
    return("R & B")
  }
  if (grepl("hip hop", word, fixed = TRUE)){
    return("Hip Hop")
  }
  else {
    return("Other")
  }
}

#prep data
chosen_year <- 1970
billboard_data <- billboard %>% 
  mutate(week_date = as.Date(week_id, format="%m/%d/%Y")) %>% 
  filter(year(week_date) == chosen_year) %>% 
  left_join(audio, by="song_id") %>%
  mutate(week_num = week(week_date)) 
main_genre = sapply(billboard_data$spotify_genre, function(x) unlist(strsplit(str_sub(x, 3, -3), split = "\', \'"))[1])
billboard_data$main_genre <- main_genre
genre <- unname(sapply(billboard_data$main_genre, function(x) select_genre(x)))
billboard_data$genre <- genre

#switch some to other = gray, na=black
genre_plot <- ggplot() +
  geom_tile(data=billboard_data, mapping=aes(x=week_num, y=week_position, fill = genre), colour="black") + 
  scale_y_reverse(breaks=c(0,25,50,75,100), labels=c(1,25,50,75,100)) +
  xlim(1,52) +
  labs(x="Week of Year\n", y="Chart Position", caption="N. Rennie | Data: Data.World / Billboard.com / Spotify.", 
       title=paste("\nBillboard 100\n\n", chosen_year, "\n", sep=""), 
       subtitle="") + 
  scale_fill_manual("", values=c("Other"="#ebcdfe", "Country"="#fec202", "Hip Hop"="#75bdfe", "R & B"="#3619e5", "Rock"="#ff1e79", "Pop"="#f172e7", "Rap"="#02dd53", "NA"="black"), 
                    na.value="black", 
                    breaks=c("Pop", "Country", "Hip Hop", "R & B", "Rock", "Rap","Other")) +
  theme(plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(fill = "black", colour="black"), 
        legend.text =  element_text(colour = 'white', size=12, family="Segoe UI"),
        plot.title = element_text(colour = 'white', size=24, face="bold", hjust = 0.5, family="Ravie"),
        plot.subtitle = element_text(colour = 'white', size=14, hjust = 0.5, family="Segoe UI"),
        plot.caption = element_text(colour = 'white', size=12, hjust = 0.5, family="Segoe UI"),
        legend.position="top",
        legend.key.size = unit(0.4, 'cm'),
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = 'white', size=12, hjust = 0.5, family="Segoe UI"),
        axis.text = element_text(colour = 'white', size=12, hjust = 0.5, family="Segoe UI"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())
genre_plot


#animate 
anim_function <- function(chosen_year){
  billboard_data <- billboard %>% 
    mutate(week_date = as.Date(week_id, format="%m/%d/%Y")) %>% 
    filter(year(week_date) == chosen_year) %>% 
    left_join(audio, by="song_id") %>%
    mutate(week_num = week(week_date)) 
  main_genre = sapply(billboard_data$spotify_genre, function(x) unlist(strsplit(str_sub(x, 3, -3), split = "\', \'"))[1])
  billboard_data$main_genre <- main_genre
  genre <- unname(sapply(billboard_data$main_genre, function(x) select_genre(x)))
  billboard_data$genre <- genre
  #switch some to other = gray, na=black
  genre_plot <- ggplot() +
    geom_tile(data=billboard_data, mapping=aes(x=week_num, y=week_position, fill = genre), colour="black") + 
    scale_y_reverse(breaks=c(0,25,50,75,100), labels=c(1,25,50,75,100)) +
    xlim(1,52) +
    labs(x="Week of Year\n", y="Chart Position", caption="N. Rennie | Data: Data.World / Billboard.com / Spotify.", 
         title=paste("\nBillboard 100\n\n", chosen_year, "\n", sep=""), 
         subtitle="") + 
    scale_fill_manual("", values=c("Other"="#ebcdfe", "Country"="#fec202", "Hip Hop"="#75bdfe", "R & B"="#3619e5", "Rock"="#ff1e79", "Pop"="#f172e7", "Rap"="#02dd53", "NA"="black"), 
                      na.value="black", 
                      breaks=c("Pop", "Country", "Hip Hop", "R & B", "Rock", "Rap","Other")) +
    theme(plot.background = element_rect(fill = "black", colour="black"),
          panel.background = element_rect(fill = "black", colour="black"),
          legend.background = element_rect(fill = "black"),
          legend.key = element_rect(fill = "black", colour="black"), 
          legend.text =  element_text(colour = 'white', size=12, family="Segoe UI"),
          plot.title = element_text(colour = 'white', size=24, face="bold", hjust = 0.5, family="Ravie"),
          plot.subtitle = element_text(colour = 'white', size=14, hjust = 0.5, family="Segoe UI"),
          plot.caption = element_text(colour = 'white', size=12, hjust = 0.5, family="Segoe UI"),
          legend.position="top",
          legend.key.size = unit(0.4, 'cm'),
          plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
          axis.title= element_text(colour = 'white', size=12, hjust = 0.5, family="Segoe UI"),
          axis.text = element_text(colour = 'white', size=12, hjust = 0.5, family="Segoe UI"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank())
  return(genre_plot)
}

#save plots
for(i in 1970:2020){
  k <- anim_function(i)
  ggsave(k, filename = paste(i, ".jpg", sep=""),  bg = "transparent")
}

#animate gif of progress
imgs <- list.files(full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
anim <- image_animate(image_joined, fps = 4)
image_write(image = anim,path = "14092021.gif")