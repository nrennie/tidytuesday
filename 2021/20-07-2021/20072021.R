library(tidyverse)
library(ggbump)
library(extrafont)

tuesdata <- tidytuesdayR::tt_load('2021-07-20')
drought <- tuesdata$drought

#2021 data
drought_CA_now <- filter(drought, state_abb == "CA" & map_date == "20210713")
drought_CA_now$x = 0
drought_CA_now$xend = 8
drought_CA_now$y = 3.5
drought_CA_now$yend = 1:6
drought_CA_now$level_perc <- rev(c(drought_CA_now$pop_pct[6],diff(rev(drought_CA_now$pop_pct[2:6])), drought_CA_now$pop_pct[1]))

#2001 data
drought_CA_then <- filter(drought, state_abb == "CA" & map_date == "20010717")
drought_CA_then$x = 0
drought_CA_then$xend = -8
drought_CA_then$y = 3.5
drought_CA_then$yend = 1:6
drought_CA_then$level_perc <- drought_CA_then$pop_pct


p <- ggplot() +
  geom_sigmoid(data=drought_CA_now[3:6,], mapping=aes(x = x, y = y, xend = xend, yend = yend, size = level_perc, colour=drought_lvl, group = factor(drought_lvl, levels=c("None", "D0", "D1", "D2", "D3", "D4"))),
               direction = "x", smooth = 6) +
  geom_sigmoid(data=drought_CA_then[1:5,], mapping=aes(x = x, y = y, xend = xend, yend = yend, size = level_perc, colour=drought_lvl, group = factor(drought_lvl, levels=c("None", "D0", "D1", "D2", "D3", "D4"))),
               direction = "x", smooth = 6) +
  xlim(-9,9) +
  annotate("text", x=-8, y=7, label="2001", colour="#bd0026", size=7, family="Maiandra GD", fontface=2) +
  annotate("text", x=8, y=7, label="2021", colour="#bd0026", size=7, family="Maiandra GD", fontface=2) +
  labs(title = "California Droughts", caption = "\nN. Rennie | Data: U.S. Drought Monitor", 
       subtitle="In just 20 years, the percentage of California's population\nexperiencing extreme or exceptional drought (level D3 or D4)\nin the second week of July has risen from 0.56% to 78.47%.") +
  geom_text(data=drought_CA_now, mapping=aes(x = xend, y = yend, label = drought_lvl, colour = drought_lvl), size = 6, nudge_x = 0.5, hjust = 0, family="Maiandra GD") +
  geom_text(data=drought_CA_then, mapping=aes(x = xend, y = yend, label = drought_lvl, colour = drought_lvl), size = 6, nudge_x = -0.5, hjust = 1, family="Maiandra GD") +
  geom_text(data=drought_CA_now, mapping=aes(x = xend-8, y = yend, label = c("No drought", "Abnormally dry", "Moderate drought", "Severe drought", "Extreme drought", "Exceptional drought"), 
                                             colour = drought_lvl), size = 6, nudge_x = 0, hjust = 0.5, family="Maiandra GD") +
  scale_colour_manual("", values=c("None"="#feb24c", "D0"="#fd8d3c", "D1"="#fc4e2a", "D2"="#e31a1c", "D3"="#bd0026", "D4"="#800026")) +
  theme(plot.background = element_rect(fill = "gray97", colour="gray97"),
        panel.background = element_rect(fill = "gray97", colour="gray97"),
        legend.position="none",
        plot.title = element_text(colour = "#bd0026", size=28, hjust = 0.5, family="Forte"),
        plot.subtitle = element_text(colour = "#bd0026", size=16, hjust = 0.5, family="Maiandra GD"),
        plot.caption = element_text(colour = "#bd0026", size=12, hjust = 0.5, family="Maiandra GD"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.6, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

