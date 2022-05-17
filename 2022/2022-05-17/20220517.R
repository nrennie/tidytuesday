library(tidyverse)
library(showtext)

# load fonts
font_add_google(name = "Gravitas One", family = "gravitas")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# get data
tuesdata <- tidytuesdayR::tt_load(2022, week = 20)
eurovision <- tuesdata$eurovision
eurovision_votes <- tuesdata$`eurovision-votes`

# prep data
eurovision_data <- eurovision_votes %>% 
  filter(from_country %in% c("France", "Germany", "Italy", "Spain", "United Kingdom"), 
         semi_final == "f", 
         jury_or_televoting == "J") %>% 
  select(year, from_country, to_country, points) %>% 
  group_by(from_country, to_country) %>% 
  summarise(total = sum(points))

# recode non big 5 to other
eurovision_data$to_country[!eurovision_data$to_country %in% c("France", "Germany", "Italy", "Spain", "United Kingdom")] = -999
eurovision_data$to_country_new = factor(eurovision_data$to_country, exclude = -999)

# save as CSV
write.csv(eurovision_data, "eurovision.csv")

#choose colours
cols_choice = c("#FF10F0", "#0062FF", "#FFFF00", "#00FF33", "#CC00FF")

# plot 
ggplot(data = eurovision_data, 
       mapping = aes(x = from_country, y = total, fill = to_country_new)) +
  geom_col(position = "fill", width = 0.7) +
  labs(x = "", 
       y = "Percentage of total votes", 
       title = "EUROVISION", 
       subtitle = str_wrap("Five countries, alongside the host country, automatically qualify for Eurovision each year - but who do they give their points to? Around 20% of points given from the `Big 5` have been given to other members of the `Big 5`, although the United Kingdom doesn't follow the trend quite so much.", 80), 
       caption = "N. Rennie | Data: Eurovision") +
  coord_cartesian(expand = F) +
  scale_y_reverse(breaks = c(1, 0.5, 0), labels = c(0, "50%", "100%")) +
  scale_fill_manual(values = cols_choice, 
                    na.value = "#565656") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text.x = element_text(colour = cols_choice, hjust = 0.5, size = 14, face = "bold"), 
        legend.position = "none", 
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"), 
        axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 1.7, 0.5, 0.5), "cm"), 
        plot.title = element_text(hjust = 0.5, family = "gravitas", size = 36, colour = "white"), 
        plot.subtitle = element_text(hjust = 0.5, family = "ubuntu", size = 13, colour = "white", 
                                     margin = margin(t = 10, b = 20)), 
        plot.caption = element_text(hjust = 0.5, family = "ubuntu", size = 12, colour = "white", 
                                     margin = margin(t = 10)), 
        axis.text.y = element_text(hjust = 0.5, family = "ubuntu", size = 12, colour = "white"), 
        axis.title.y = element_text(hjust = 0.5, family = "ubuntu", size = 12, colour = "white") 
  )


