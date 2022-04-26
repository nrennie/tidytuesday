library(tidyverse)
library(showtext)
library(plotly)
library(tidytext)
library(lubridate)
library(usefunc)
library(htmlwidgets)

# add fonts
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# get data
tuesdata <- tidytuesdayR::tt_load(2022, week = 17)
hidden_gems <- tuesdata$hidden_gems

# data for sentiment analysis
data(stop_words)
afinn_df <- get_sentiments("afinn")

# average sentiment of title by date
text_df <- hidden_gems %>%
  select(date, title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word")

sent_df <- text_df %>%
  inner_join(afinn_df, by = "word") %>%
  group_by(date) %>%
  summarise(mean_sent = mean(value, na.rm = T)) %>%
  drop_na()

# plot
p <- ggplot(data = sent_df,
            mapping = aes(x = date, y = mean_sent)) +
  geom_rect(aes(xmin = min(date), xmax = max(date), ymin = 0, ymax = 5),
            fill = "#5BB381") +
  geom_rect(aes(xmin = min(date), xmax = max(date), ymin = -5, ymax = 0),
            fill = "#4A154B") +
  geom_point(colour = "white", size = 2) +
  geom_smooth(method = 'loess', fill = "white", colour = "black") +
  annotate("text", x = dmy("15032021"), y = 4.5,
           label = "POSITIVE", hjust = 1,
           family = "ubuntu", colour = "#fafafa", size = 6) +
  annotate("text", x = dmy("15032021"), y = -4.5,
           label = "NEGATIVE", hjust = 1,
           family = "ubuntu", colour = "#fafafa", size = 6) +
  coord_cartesian(expand = F) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(x = "",
       y = "Average Sentiment",
       title = "Kaggle Hidden Gems",
       subtitle = str_wrap_break("Sentiment analysis of Kaggle Hidden Gems titles using {tidytext} reveals an overall positive sentiment, and a slight increasing trend. The sentiment value lies between -5 and 5, with -5 meaning very negative sentiment and 5 being very positive sentiment.", 100),
       caption = "N. Rennie | Data: Kaggle") +
  theme(plot.margin = unit(c(0.5, 1.2, 0.5, 0.5), "cm"),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"),
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"),
        plot.title = element_text(family = "ubuntu", hjust = 0.5, size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0.5,
                                     size = 12, color = "black",
                                     margin = margin(t = 10, b = 10)),
        plot.caption = element_text(family = "ubuntu", hjust = 0.5, size = 12, color = "black"),
        axis.text = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black"),
        axis.title = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black"),
        axis.ticks.y = element_blank())
p