library(tidyverse)
library(showtext)
library(usefunc)

# get data
tuesdata <- tidytuesdayR::tt_load('2022-05-24')
sevens <- tuesdata$sevens
fifteens <- tuesdata$fifteens

# load fonts
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# prep data
plot_data <- fifteens %>% 
  filter(tournament == "6 Nations") %>% 
  filter(team_1 == "Scotland" | team_2 == "Scotland") %>% 
  select(date, team_1, team_2, margin_of_victory, winner) %>% 
  mutate(value = if_else(winner != "Scotland", -1*margin_of_victory, margin_of_victory)) %>% 
  mutate(id = row_number()) %>% 
  mutate(home = as.numeric((team_1 == "Scotland")))

# breaks for years
breaks_c = plot_data %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(year %in% c(2002, 2012, 2022)) %>% 
  group_by(year) %>% 
  slice(n = 1) %>% 
  pull(id)

#subtitle
st <- str_wrap_break("Scotland's Women's rugby team have won just over 20% of their matches in the 20 year period between 2002 and 2022. They ended their 34 match losing streak in February 2017, winning 15–14 against Wales. \n\n N. Rennie | Data: ScrumQueens", 80)

# plot
p <- ggplot(data = plot_data) +
  geom_segment(aes(x = id, xend = id, y = value, yend = 0), 
               size = 2, colour = alpha("navy", 0.3)) +
  geom_point(aes(x = id, y = value, colour = as.factor(home)), 
             size = 3) +
  geom_hline(yintercept = 0, colour = "navy") +
  annotate("text", x = 3, y = -80, label = "LOSE", colour = "navy", family = "ubuntu") +
  annotate("text", x = 3, y = 80, label = "WIN", colour = "navy", family = "ubuntu") +
  scale_y_continuous(limits = c(-100, 100)) +
  scale_x_continuous(breaks = breaks_c, 
                     labels = c(2002, 2012, 2022)) +
  scale_colour_manual("", 
                      values = c("0" = "#C74893", "1" = "#01965A"), 
                      labels = c("Home", "Away"), 
                      breaks = c(1, 0)) +
  guides(colour=guide_legend(ncol=2)) +
  labs(x = "", y = "Margin of Victory", 
       title = "Women's Six Nations Championship: Scotland", 
       subtitle = st) +
  theme(plot.title = element_text(hjust = 1, vjust = -8, 
                                  colour = "navy", face = "bold", 
                                  size = 24, family = "ubuntu"), 
        plot.subtitle = element_text(hjust = 1, vjust = -20, 
                                     size = 12, lineheight = 0.8,
                                     colour = "navy", family = "ubuntu"), 
        axis.text = element_text(colour = "navy", family = "ubuntu", 
                                   size = 10, lineheight = 0.4),
        axis.title = element_text(colour = "navy", family = "ubuntu", 
                                 size = 10, lineheight = 0.4),
        legend.text = element_text(colour = "navy", family = "ubuntu", 
                                  size = 10, lineheight = 0.4),
        legend.title = element_blank(), 
        legend.position = c(0.93, 0.8),
        panel.grid = element_blank(),
        legend.key = element_rect(fill = "#fafafa", colour = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0, 0.7, 0.5, 0.5), "cm"))
p





