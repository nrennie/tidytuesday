library(tidyverse)
library(showtext)
library(lubridate)
library(ggchicklet)
library(rcartocolor)
library(usefunc)
library(forcats)

# add fonts
font_add_google("Bubbler One", "bubbler")
showtext_auto()

# read data
frog <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frog.csv')

# sort structures
struc_levs <- frog %>% 
  mutate(SurveyDate = mdy(SurveyDate)) %>% 
  mutate(n = 1) %>% 
  select(Structure, n) %>% 
  group_by(Structure) %>% 
  summarise(total = sum(n)) %>% 
  arrange(-total) %>% 
  pull(Structure)

# sort sites
site_levs <- frog %>% 
  mutate(n = 1) %>% 
  select(Subsite, n) %>% 
  group_by(Subsite) %>% 
  summarise(total = sum(n)) %>% 
  arrange(-total) %>% 
  pull(Subsite)

# prep data
plot_data <- frog %>% 
  mutate(SurveyDate = mdy(SurveyDate)) %>% 
  mutate(n = 1) %>% 
  select(Subsite, Structure, n) %>% 
  group_by(Subsite, Structure) %>% 
  summarise(total = sum(n)) %>% 
  mutate(Structure = factor(Structure, levels = struc_levs), 
         Subsite = factor(Subsite, levels = rev(site_levs)), 
         Subsite = fct_recode(Subsite,
                              `North East Reservoir` = "NE Res",
                              `North Reservoir` = "N Res", 
                              `West Reservoir` = "W Res", 
                              `South East Pond` = "SE Pond"))

# subtitle
st <- str_wrap_break("Radio-telemetry was used to study Oregon spotted frogs at Crane Prairie Reservoir in Oregon between September and late November of 2018. Oregon spotted frogs were most commonly found in areas with herbaceous vegetation.\n\nN. Rennie | Data: USGS.gov", 120)

# plot
ggplot(plot_data, 
       aes(y = total, x = Subsite, fill = Structure)) +
  geom_chicklet(colour = "#F8F0C6", radius = grid::unit(4, "pt")) +
  coord_flip() +
  labs(title = "OREGON SPOTTED FROG", 
       subtitle = st, 
       x = "", 
       y = "\nNumber of frogs observed") +
  scale_fill_carto_d(palette = "Antique") +
  theme(legend.position = "top", 
        legend.title = element_blank(), 
        plot.margin = unit(c(0.5, 3, 0.5, 0.5), "cm"), 
        plot.title = element_text(hjust = 0.5, family = "bubbler", size = 26, 
                                  margin = margin(b = 10)), 
        plot.background = element_rect(colour = "#F8F0C6", fill = "#F8F0C6"), 
        panel.background = element_rect(colour = "#F8F0C6", fill = "#F8F0C6"), 
        legend.background = element_rect(colour = "#F8F0C6", fill = "#F8F0C6"), 
        legend.key = element_rect(colour = "#F8F0C6", fill = "#F8F0C6"),
        legend.text = element_text(family = "bubbler", size = 12),
        plot.subtitle = element_text(family = "bubbler", size = 12, hjust = 0.5),
        axis.text = element_text(family = "bubbler", size = 12),
        axis.title = element_text(family = "bubbler", size = 12),
        axis.ticks = element_blank(), 
        panel.grid = element_blank())
