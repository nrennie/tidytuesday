library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-03-02')
youtube <- tuesdata$youtube

#function for percentages
round_percent <- function(x) { 
  x <- x/sum(x)*100  # Standardize result
  res <- floor(x)    # Find integer bits
  rsum <- sum(res)   # Find out how much we are missing
  if(rsum<100) { 
    # Distribute points based on remainders and a random tie breaker
    o <- order(x%%1, sample(length(x)), decreasing=TRUE) 
    res[o[1:(100-rsum)]] <- res[o[1:(100-rsum)]]+1
  } 
  res 
}

#aggregate by brand
ads <- youtube %>%
  group_by(brand) %>%
  summarise(funny = sum(funny), patriotic = sum(patriotic), celebrity=sum(celebrity), 
            danger=sum(danger), animals=sum(animals), use_sex=sum(use_sex))

num_boxes <- data.frame(cbind(brand=ads$brand, apply(ads[,2:7], 2, function(x) round_percent(x/sum(x)))))
brand_boxes <- data.frame(apply(num_boxes[,2:7],2, function(x) rep(ads$brand, x)))
long_data <- gather(brand_boxes, key="measure", value="value", c("funny", "patriotic", "celebrity", "danger", "animals", "use_sex"))
long_data$x <- rep(1:10,10)
long_data$y <- rep(1:10,rep(10,10))

category_names <- list(
  'funny'="Funny",
  'patriotic'="Patriotic",
  'danger'="Danger",
  'use_sex'="Sex",
  'celebrity'="Celebrity",
  'animals'="Animals"
)

category_labeller <- function(variable,value){
  return(category_names[value])
}

p <- ggplot(long_data, aes(x, y, width=.7, height=.5)) +
  geom_tile(aes(fill = value), colour = "grey85") + 
  facet_wrap(~measure, labeller=category_labeller) +
  labs(title="Superbowl Commercials", subtitle="Data on the 233 commercials from the 10 brands that aired the most spots in all\n21 Super Bowls this century. The boxes represent the percentage of each category\nof commercial which was made by each brand.", caption="N. Rennie | Data source: FiveThirtyEight") +
  scale_fill_manual("", values=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')) +
  theme(panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "grey85"),
        legend.background = element_rect(fill = "grey85"),
        legend.position="right",
        plot.title = element_text(size = 14, face=2),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8, hjust=0),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank()
  )

p

ggsave(p, filename = "02032021.jpg")






