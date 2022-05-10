library(tidyverse)
library(showtext)
library(usefunc)

# load fonts
font_add_google(name = "Mate SC", family = "mate")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# load data 
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')

# prep data
most_books <- nyt_titles %>% 
  group_by(author) %>% 
  summarise(total_books = n()) %>% 
  slice_max(total_books, n = 10) %>% 
  pull(author)
most_books <- factor(most_books, levels = most_books)

plot_data <- nyt_titles %>% 
  select(author, title, year) %>% 
  filter(author %in% most_books) %>% 
  arrange(author, year) %>% 
  group_by(author) %>% 
  mutate(num = row_number(), 
         author = factor(author, levels = levels(most_books))) %>% 
  select(-year)

total_books <- plot_data %>% 
  select(-title) %>% 
  group_by(author) %>% 
  slice_max(num, n = 1)

# subtitle
st <- str_wrap_break("The New York Times Best Seller list is widely considered the preeminent list of best-selling books in the United States. It has been published weekly since 1931. Danielle Steel is the author with the highest number of books on the NYT Best Seller list with 116 books making the list, almost double that of second place author, Stuart Woods.\n\nN. Rennie | Data: Post45 Data", 100)

# plot
p <- ggplot() +
  geom_text(data = plot_data, 
            mapping = aes(x = author, y = num, label = title), 
            size = 3, colour = "white", family = "ubuntu") +
  geom_text(data = total_books, 
            mapping = aes(x = author, y = num+4, label = num), 
            size = 7, colour = "white", family = "ubuntu", fontface = "bold") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x = "", 
       y = "", 
       title = "The New York Times Best Seller List", 
       subtitle = st) +
  theme(plot.title = element_text(hjust = 1, vjust = -8, 
                                  colour = "white", face = "bold", 
                                  size = 68, family = "mate"), 
        plot.subtitle = element_text(hjust = 1, vjust = -40, 
                                     size = 20, lineheight = 0.4,
                                     colour = "white", family = "ubuntu"), 
        axis.text.x = element_text(colour = "white", family = "ubuntu", 
                                   size = 20, lineheight = 0.4), 
        axis.text.y = element_blank(),
        axis.title = element_blank(), 
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"), 
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(-1, 0.7, 0.5, 0.5), "cm"))

ggsave(p, filename = "2022/2022-05-10/20220510.png", height = 1600, width = 2400, unit = "px")
