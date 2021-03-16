library(tidyverse)
library(wordcloud2)
library(magick)
library(cowplot)
library(RColorBrewer)
library(tm)

tuesdata <- tidytuesdayR::tt_load('2021-03-16')
games <- tuesdata$games

#look at game names
gamenames <- unique(games$gamename)
text <- unlist(sapply(gamenames, function(x) strsplit(x, " ")))
docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

#make wordcloud and save as png
set.seed(1234) 
max_words <- 600
pass_to_wordcloud <- df[1:max_words, ]
wordcloud2(pass_to_wordcloud, size=1.6, color = rep_len(rev(brewer.pal(8, "YlOrRd")), nrow(pass_to_wordcloud)), backgroundColor="lightgrey")

img <- "wordcloud.png" %>%
  image_read() %>%
  image_resize("400x400") %>%
  image_colorize(30, "white")

#plot bar chart
most_popular <- df[c(1:4,6),]
p <- ggplot(data=most_popular, aes(x=factor(word, levels=rev(most_popular[,1])), y=freq)) +
  geom_bar(stat="identity", fill="red4")+
  geom_text(aes(label=freq), hjust=1.6, color="lightgrey", fontface=2, size=7)+
  geom_text(aes(label=word, x=word, y=1), hjust=0, color="gold2", fontface=2, size=7)+
  coord_flip() +
  labs(title="    Most Common Words in Game Titles", 
       caption="\n\nN. Rennie | Data source: Steam") +
  theme_minimal_hgrid() +
  theme( plot.title = element_text(size = 20, face=2, colour="gold2"),
         plot.title.position = "plot",
         plot.subtitle = element_text(size = 10, colour="gold2"),
         plot.caption = element_text(size = 8, hjust=1, colour="gold2"),
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

ggdraw() + 
  theme(plot.background = element_rect(fill="red4", color = NA)) +
  draw_image(img) + 
  draw_plot(p)

