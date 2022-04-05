library(tidyverse)
library(showtext)
library(usefunc)

tuesdata <- tidytuesdayR::tt_load('2022-04-05')
news_orgs <- tuesdata$news_orgs


# load fonts
font_add_google(name = "Red Rose", family = "rose")
showtext_auto()


# prep data
plot_data <- news_orgs %>% 
  filter(!is.na(year_founded), 
         year_founded >= 1970,
         country == "United States") %>% 
  mutate(year_founded = as.character(year_founded)) %>% 
  group_by(year_founded) %>% 
  summarise(count = n()) 

date_data <- tibble(year_founded = as.character(1970:2021))

plot_data2 <- 
  left_join(date_data, plot_data, by=c("year_founded")) %>% 
  replace_na(list(count = 0)) %>% 
  arrange(year_founded) %>% 
  mutate(nsum = cumsum(count)) 


# subtitle
st <- str_wrap_break("The number of digitally focused, local news organizations in the United States has grown dramatically since 2010. \n\nN. Rennie | Data: Project Oasis", 50)


# plot
p <- ggplot(data = plot_data2, 
            mapping = aes(x = as.numeric(year_founded), y = nsum)) +
  geom_area(fill = "#8b0000") +
  labs(title = "News Publications", 
       subtitle = st,
       x = "", 
       y = "") +
  coord_cartesian(expand = F, ylim = c(0, 680)) +
  scale_x_continuous(position = "top") +
  theme(plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-5.5, 0, 0, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text =  element_text(colour = "#8b0000", size=10, hjust = 0.5, family="rose"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(hjust = 0.7, margin = margin(t = -30)),
        plot.title = element_text(colour = "#8b0000", size=26, face = "bold", hjust = 0, 
                                  vjust = -40, family="rose", margin = margin(10, 0, 20, 0)),
        plot.subtitle = element_text(colour = "#8b0000", size=14, hjust = 0, vjust = -80, 
                                     family="rose", margin = margin(10, 0, 20, 0))
  )

p
