library(tidyverse)
library(usefunc)
library(showtext)
library(emojifont)

# get data
tuesdata <- tidytuesdayR::tt_load('2022-03-08')
erasmus <- tuesdata$erasmus

# load fonts
font_add_google(name = "Bodoni Moda", family = "Bodoni MT")
showtext_auto()

# prep data
plot_data <- erasmus %>% 
  select(academic_year, fewer_opportunities, participants) %>% 
  group_by(academic_year, fewer_opportunities) %>% 
  summarise(n = sum(participants, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(academic_year) %>% 
  mutate(year_n = sum(n), 
         year_perc = round(100*n/year_n))

# prep waffle data
waffle_data <- rep_df(expand.grid(x = rep(1:10), y = rep(1:10)), length(unique(plot_data$academic_year))) %>%
  mutate(year = rep(unique(plot_data$academic_year), each = 100),
         label = fontawesome('fa-graduation-cap'),
         type = rep(plot_data$fewer_opportunities, times = plot_data$year_perc))

# facet plot
ggplot() +
  geom_text(data = waffle_data,
            mapping = aes(x = x,
                          y = y,
                          label = label,
                          colour = type),
            family='fontawesome-webfont', size = 4) +
  facet_wrap(~year, nrow = 1) +
  scale_colour_manual("", values = c("#d3d3d3", "#673eb7")) +
  labs(title = str_wrap_break("Erasmus: what % of students are from backgrounds with fewer opportunities?", 75),
       subtitle = str_wrap_break("The percentage of students from backgrounds with fewer opportunities participating in the Erasmus exchange programme peaked at 21% in the 2016-2017 academic year.\n\nN. Rennie | Data: Data.Europa\n\n", 120),
       x = "",
       y = "") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = "#e7e7e7", colour = "#e7e7e7"),
        plot.background = element_rect(fill = "#e7e7e7", colour = "#e7e7e7"),
        legend.position="none",
        strip.background =element_rect(fill="#e7e7e7", colour ="#e7e7e7"),
        strip.text = element_text(colour = '#404040', family="Bodoni MT", size=12),
        plot.title = element_text(colour = "#404040", size=26, hjust = 0, family="Bodoni MT"),
        plot.subtitle = element_text(colour = "#404040", size=12, hjust = 0, family="Bodoni MT"),
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank())

