library(tidyverse)
library(geofacet)
library(usefunc)
library(showtext)

# get data
tuesdata <- tidytuesdayR::tt_load('2022-02-22')
freedom <- tuesdata$freedom

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# prep data
africa_data <- freedom %>%
  filter(Region_Name == "Africa") %>% 
  mutate(country = recode(country, 
                          "CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire" = "Côte d'Ivoire", 
                          "Sao Tome and Principe" = "São Tomé and Principe", 
                          "United Republic of Tanzania" = "Tanzania", 
                          "Congo" = "Republic of the Congo")) %>% 
  select(country, year, PR) 

plot_2020 <- africa_data %>% 
  filter(year == 2020) %>% 
  select(country, PR)

plot_data <- left_join(africa_data, plot_2020, by = "country")

# plot 
ggplot(plot_data, aes(x = year, y = PR.x, fill = as.character(PR.y))) +
  geom_area() +
  facet_geo(~ country, grid = africa_countries_grid1, label = "code") +
  scale_x_continuous(limits = c(1995, 2020), breaks = c(2000, 2020)) +
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10)) +
  coord_cartesian(expand = F) +
  labs(title = "Freedom in the World: Political Rights", 
       subtitle = str_wrap_break("Freedom in the World, Freedom House's flagship publication, is the standard-setting comparative assessment of global political rights and civil liberties. Published annually since 1972, the survey ratings and narrative reports on 195 countries and 15 related and disputed territories are used by policymakers, the media, international corporations, civic activists, and human rights defenders.", 70), 
       caption = "N. Rennie | Data: Freedom House", 
       x = "", 
       y = "") +
  scale_fill_brewer("2020 Political\nRights Index", palette = "Set1") +
  guides(fill=guide_legend(ncol=2)) +
  theme_light() +
  theme(plot.background = element_rect(fill = "#dfd3c2", colour="#dfd3c2"),
        panel.background = element_rect(fill = "#dfd3c2", colour="#dfd3c2"), 
        plot.title = element_text(colour = "black", size=16, face = "bold", hjust = 0.5, family="space", 
                                  margin = margin(0, 0, 10, 0)), 
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0.5, family="space", 
                                     margin = margin(0, 0, 30, 0)), 
        plot.caption = element_text(colour = "black", size=10, hjust = 0.5, family="space", 
                                     margin = margin(5, 0, 5, 0)), 
        strip.text = element_text(colour = "black", size=9, hjust = 0.5, family="space"), 
        strip.background = element_rect(fill = "#dfd3c2", colour="#dfd3c2"), 
        plot.margin = unit(c(0.3, 0.9, 0.3, 0.3), "cm"), 
        legend.background = element_rect(fill = "#dfd3c2", colour="#dfd3c2"), 
        legend.key = element_rect(colour="#dfd3c2", fill = "#dfd3c2"),
        legend.position = c(0.85, 0.1),
        legend.text = element_text(colour = "black", size=7, hjust = 0.5, family="space"), 
        legend.title = element_text(colour = "black", size=8, hjust = 0.5, family="space"))
