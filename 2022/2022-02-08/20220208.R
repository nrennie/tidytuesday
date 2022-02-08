library(tidyverse)
library(usefunc)
library(lubridate)
library(rcartocolor)
library(showtext)

# get data
tuesdata <- tidytuesdayR::tt_load('2022-02-08')
airmen <- tuesdata$airmen

# load fonts
font_add_google(name = "Red Rose", family = "rose")
showtext_auto()

# prep data
airmen_data <- airmen %>% 
  select(graduation_date, rank_at_graduation) %>% 
  filter(!is.na(rank_at_graduation), 
         rank_at_graduation %notin% c("N/A", "Unk"), 
         !is.na(graduation_date)) %>% 
  mutate(rank_at_graduation = factor(rank_at_graduation), 
         rank_at_graduation = fct_recode(rank_at_graduation, "Captain" = "Capt"), 
         rank_at_graduation = fct_relevel(rank_at_graduation, "Flight Officer", "2nd Lt", "1st Lt", "Captain"), 
         monthy = floor_date(graduation_date, "month")) %>% 
  group_by(rank_at_graduation, monthy) %>% 
  summarize(nmonth = n()) %>% 
  arrange(monthy) 

date_data <- tibble(expand.grid(monthy = seq(min(airmen_data$monthy),max(airmen_data$monthy),by='months'), 
                         "rank_at_graduation" = unique(airmen_data$rank_at_graduation)))
  
plot_data <- 
  left_join(date_data, airmen_data, by=c("monthy", "rank_at_graduation")) %>% 
  replace_na(list(nmonth = 0)) %>% 
  arrange(monthy) %>% 
  group_by(rank_at_graduation) %>% 
  mutate(nsum = cumsum(nmonth)) 

# for subtitle info
airmen %>% 
  filter(rank_at_graduation %in% c("Capt", "Captain")) %>% 
  select(name)

# plot 
p <- ggplot(data = plot_data, 
            mapping = aes(x = monthy, y = nsum, fill = rank_at_graduation)) +
  geom_area() +
  labs(title = "Tuskegee Airmen", 
       x = "", 
       y = "", 
       subtitle = str_wrap_break("The Tuskegee Airmen were the first African-American military aviators in the United States Armed Forces. This plot shows the cumulative number graduates of each rank. Only William H. Shannon and Benjamin O. Davis. Jr graduated with the rank of Captain.\n\nN. Rennie | Data: Tuskegee Airmen Challenge", 60)) +
  coord_cartesian(expand = F) +
  scale_fill_carto_d(palette = "Antique") +
  theme(plot.background = element_rect(fill = "gray90", colour="gray90"),
        panel.background = element_rect(fill = "gray90", colour="gray90"),
        legend.background = element_rect(fill = "gray90", colour="gray90"),
        legend.position = c(0.09, 0.65), 
        legend.title = element_blank(),
        legend.key = element_rect(fill = "gray90", colour = "gray90"),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-3, 0, 0, 0), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text =  element_text(colour = "black", size=14, hjust = 0.5, family="rose"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(hjust = -0.7, margin = margin(t = -30)),
        legend.text = element_text(colour = "black", size=12, hjust = 0, family="rose"),
        plot.title = element_text(colour = "black", size=26, face = "bold", hjust = 0, vjust = -15, family="rose", margin = margin(10, 0, 20, 0)),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0, vjust = -35, family="rose", margin = margin(10, 0, 20, 0))
        )
p


