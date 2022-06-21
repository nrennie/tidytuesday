library(tidyverse)
library(showtext)
library(usefunc)
library(ggrepel)

# get data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
firsts

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# prep data
plot_data <- firsts %>% 
  filter(gender == "Female African American Firsts", 
         category == "Politics") %>% 
  # remove items in wrong category / gender
  filter(person %notin% c("Fred Luter[234][235]", 
                          "Stephen Rochon[224]", 
                          "Cheryl White[194]")) %>% 
  mutate(name = sub("\\[.*", "", person)) %>% 
  mutate(name = sub("\\(.*", "", name)) %>% 
  mutate(name = sub("\\,.*", "", name)) %>% 
  mutate(name = sub("\\of.*", "", name)) %>% 
  mutate(name = stringr::str_trim(name))  %>% 
  select(-c(gender, category, person)) %>% 
  mutate(year_pos = year, 
         year_pos2 = year) %>% 
  # close together, non equal
  mutate(year_pos = ifelse(year == 2009, 2010, year_pos)) %>% 
  mutate(year_pos = ifelse(year == 1993, 1994, year_pos)) %>% 
  mutate(year_pos = ifelse(year == 1991, 1990, year_pos)) %>% 
  mutate(year_pos2 = ifelse(year == 2009, 2010, year_pos2)) %>% 
  mutate(year_pos2 = ifelse(year == 1993, 1994, year_pos2)) %>% 
  mutate(year_pos2 = ifelse(year == 1991, 1990, year_pos2)) %>% 
  # same year 
  mutate(year_pos = ifelse(name == "Jeanine Menze", 2003, year_pos)) %>% 
  mutate(year_pos = ifelse(name == "Charlene Mitchell", 1966, year_pos)) %>% 
  mutate(year_pos = ifelse(name == "Cora Brown", 1954, year_pos)) %>% 
  mutate(year_pos = ifelse(name == "Crystal Bird Fauset", 1936, year_pos))
plot_data

# plot
ggplot(data = plot_data) +
  geom_segment(mapping = aes(y = 0, yend = 0, x = min(year), xend = max(year))) +
  geom_point(mapping = aes(x = year, y = 0), 
             pch = 21, fill = "white", colour = "black", size = 4) +
  # year
  geom_text(mapping = aes(x = year_pos2, y = 1, label = year), 
            hjust = 0, family="space", size = 8) +
  #name and accomplishment
  geom_text(mapping = aes(x = year_pos, y = -1, label = name), 
            fontface = "bold", hjust = 1, family="space", size = 8) +
  geom_text(mapping = aes(x = year_pos, y = 2.5, label = accomplishment), 
            hjust = 0, family="space", size = 8) +
  coord_flip() +
  ylim(-4, 14) +
  labs(title = "African American Women in Politics", 
       subtitle = "N. Rennie | Data: #TidyTuesday 2020 Week 24") +
  theme(panel.background = element_rect(fill = "#d0bba8", colour = "#d0bba8"),
        plot.background = element_rect(fill = "#d0bba8", colour = "#d0bba8"),
        legend.background = element_rect(fill = "#d0bba8", colour = "#d0bba8"),
        plot.title = element_text(colour = "black", size=38, face="bold", hjust = 0.5, family="space"),
        plot.subtitle = element_text(colour = "black", size=20, hjust = 0.5, family="space", 
                                     margin = margin(t = 10)),
        legend.title = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ggsave(filename = "2022/2022-06-21/20220621.png", width = 8.5, height = 8, unit = "in")
