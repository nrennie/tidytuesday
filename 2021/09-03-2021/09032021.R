library(tidyverse)
library(ggridges)
tuesdata <- tidytuesdayR::tt_load('2021-03-09')
movies <- tuesdata$movies

director <- table(movies$director)
directors <- names(director)[which(director > 8)]
movies_data <- filter(movies, director %in% directors)
movies_data$test_numeric <- as.numeric(as.character(factor(movies_data$binary, levels=c("PASS","FAIL"), labels=c(1,0))))

p <- ggplot(movies_data, aes(x = test_numeric, y = director, fill = ..x..)) + geom_density_ridges_gradient(scale = 0.9) +
  scale_fill_gradient2(low="red1", high="springgreen4", mid="yellow", midpoint=0.5) +
  scale_x_continuous(breaks=c(0,1), labels=c("FAIL", "PASS")) +
  labs(title="The Bechdel Test", subtitle="For a film to pass the Bechdel test, it must:\n(i) contain at least two named women,\n(ii) have a conversation between those two women at some point, and\n(iii) that conversation isn’t about a male character.\n\nVisualised below are the distributions of passes and fails for\nfilms between 1970 and 2013, for nine directors.\n", caption="N. Rennie | Data source: FiveThirtyEight") +
  theme(panel.background = element_rect(fill = "aliceblue"),
        plot.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue"),
        legend.position="none",
        plot.title = element_text(size = 14, face=2),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8, hjust=1),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_text(size = 10, colour="black"),
        axis.text.y=element_text(size = 10, colour="black"),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank()
  )
p

ggsave(p, filename = "09032021.jpg")

