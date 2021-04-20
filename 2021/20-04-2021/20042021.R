library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix
netflix$count <- 1

#number released for different countries each year
netflix_data <- select(netflix, release_year, country, count, type)

#process country data
select_first_country <- function(country){
  pos <- which(unlist(strsplit(country, "")) == ",")[1]
  if (length(pos) == 0){
    return(country)
  }
  if (is.na(pos)){
    return(country)
  }
  else {
    return(paste(unlist(strsplit(country, ""))[1:(pos-1)], collapse=''))
  }
}
country2 <- sapply(netflix_data$country, function(x) select_first_country(x))
netflix_data$country2 <- country2
netflix_data <- drop_na(netflix_data)
netflix_data <- netflix_data %>% group_by(type, release_year, country2)
netflix_data <- netflix_data %>% summarise(count = sum(count))

#filter by year and country
netflix_data <- filter(netflix_data, release_year >= 1997)
choose_countries <- c("United States", "India", "United Kingdom", "Canada", "South Korea", "Japan", "Spain", "France", "Brazil", "Mexico")
netflix_data <- filter(netflix_data, country2 %in% choose_countries)

p <- ggplot() +
  geom_line(data = netflix_data, mapping = aes(x = release_year, y = count, group = country2, colour=country2)) +
  scale_colour_manual("", values=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')) +
  labs(title="Netflix Productions in Different Countries", 
       subtitle="After 2014, the number of movies and TV shows produced by Netflix increased\ngreatly. However, the number of movies produced has continued to drop since 2017. \n\n *where a TV show or movie was produced in multiple countries it is attributed to\nthe first listed.\n\n", 
       caption="N. Rennie | Data: Kaggle & Flixable  \n", 
       x="\nYear of Release", y="Number of TV Shows/Movies Produced\n\n") +
  facet_grid(rows = vars(type)) +
  xlim(1995, 2021) +
  guides(colour=guide_legend(ncol=5)) +
  theme(axis.text=element_text(colour = "#B81D24", size=10, hjust=0, family="Corbel Light"),
        axis.title.x = element_text(colour = "#B81D24", size=10, hjust=0.5, family="Corbel Light"),
        axis.title.y = element_text(colour = "#B81D24", size=10, hjust=0.5, family="Corbel Light"),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#F5F5F1", color = NA),
        panel.background = element_rect(fill = "#F5F5F1"),
        plot.margin = unit(c(0.8, 0.5, 0.3, 0.3), "cm"),
        strip.background =element_rect(fill="#F5F5F1"),
        strip.text = element_blank(),
        legend.background = element_rect(color = NA,fill="transparent"),
        legend.box.background = element_rect(fill = "transparent",color=NA),
        legend.position=c(0.5,0.5),legend.justification=c(0.5,0.5),
        plot.title = element_text(colour = "#B81D24", size=15, face="bold", hjust=0, family="Bodoni MT Black"),
        plot.subtitle = element_text(colour = "#221F1F", size=10, hjust=0, family="Corbel Light"),
        plot.caption = element_text(colour = "#B81D24", size = 8, hjust=1, family="Corbel Light"),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.spacing = unit(3, "lines"),
        legend.text = element_text(colour = "#B81D24", size=8, hjust=0), 
        legend.title=element_blank())

p

CairoWin(width=5.3,height=6,unit="in")

q <- ggdraw(p) +
  draw_label(label="Movies", x=0.14, y=0.7, colour = "#B81D24", size = 14, hjust=0, fontfamily="Bodoni MT Black") +
  draw_label(label="TV Shows", x=0.14, y=0.34, colour = "#B81D24", size = 14, hjust=0, fontfamily="Bodoni MT Black") +
  draw_label(label="Netflix founded\n1997", x=0.20, y=0.55, colour = "#B81D24", size = 10, hjust=0.5, fontfamily="Corbel Light") +
  draw_label(label="Netflix expands to Canada\n2010", x=0.59, y=0.58, colour = "#B81D24", size = 10, hjust=0.5, fontfamily="Corbel Light") +
  draw_label(label="Netflix IPO\n2002", x=0.32, y=0.20, colour = "#B81D24", size = 10, hjust=0.5, fontfamily="Corbel Light") +
  draw_label(label="First original content\n`House of Cards`\n2013", x=0.65, y=0.21, colour = "#B81D24", size = 10, hjust=0.5, fontfamily="Corbel Light")

q

png(filename="20042021.png", 
    type="cairo",
    units="in", 
    width=5.3, 
    height=6, 
    pointsize=12, 
    res=96)
print(q)
dev.off()
