library(tidyverse)
library(showtext)
library(usefunc)
library(tidytext)
library(lubridate)

# add fonts
font_add_google(name = "Mate SC", family = "mate")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# get data
tuesdata <- tidytuesdayR::tt_load('2022-04-19')
times <- tuesdata$times

# data for sentiment analysis
data(stop_words)
afinn_df <- get_sentiments("afinn")

# average sentiment of answer by date
text_df <- times %>% 
  select(puzzle_date, answer) %>% 
  unnest_tokens(word, answer) %>% 
  anti_join(stop_words, by = "word")

sent_df <- text_df %>% 
  inner_join(afinn_df, by = "word") %>% 
  group_by(puzzle_date) %>% 
  summarise(mean_sent = mean(value, na.rm = T)) %>% 
  drop_na() %>% 
  filter(puzzle_date > dmy("01012014"))

# plot 
ggplot(data = sent_df, 
       mapping = aes(x = puzzle_date, y = mean_sent)) +
  geom_rect(aes(xmin = dmy("01012014"), xmax = dmy("31122021"), ymin = 0, ymax = 5.1), 
            fill = "#148179") +
  geom_rect(aes(xmin = dmy("01012014"), xmax = dmy("31122021"), ymin = -5.1, ymax = 0), 
            fill = "#811453") +
  geom_point(colour = "#fafafa") +
  geom_line(colour = "#fafafa") +
  annotate("text", x = dmy("01122021"), y = 4.5, 
           label = "POSITIVE", hjust = 1, 
           family = "mate", colour = "#fafafa", size = 6) +
  annotate("text", x = dmy("01122021"), y = -4.5, 
           label = "NEGATIVE", hjust = 1, 
           family = "mate", colour = "#fafafa", size = 6) +
  coord_cartesian(expand = F) +
  labs(x  = "", 
       y = "Average Sentiment", 
       title = "The Times Crossword Puzzles", 
       subtitle = str_wrap_break("The sentiment of crossword puzzle answers has been ranked between -5 and 5, with -5 meaning very negative sentiment and 5 being very positive sentiment. The average sentiment across all puzzles is -0.43, indicating an overall slightly negative sentiment of crossword answers in The Times.", 140), 
       caption = "N. Rennie | Data: Cryptic Crossword Clues") +
  theme(plot.margin = unit(c(0.5, 1.2, 0.5, 0.5), "cm"), 
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.title = element_text(family = "mate", hjust = 0.5, size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0.5, size = 12, color = "black"),
        plot.caption = element_text(family = "ubuntu", hjust = 0.5, size = 12, color = "black"), 
        axis.text = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black"), 
        axis.title = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black"), 
        axis.ticks.y = element_blank())






