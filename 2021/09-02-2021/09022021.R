library(tidyverse)
devtools::install_github("dgrtwo/gganimate")
library(gganimate)

tuesdata <- tidytuesdayR::tt_load('2021-02-09')
wealth <- tuesdata$lifetime_wealth

median_wealth <- filter(wealth, type == "Median")
d <- median_wealth %>%  
  pivot_wider(names_from=race, values_from=wealth_lifetime) %>%  
  mutate(Black_perc = 100 * (Black / (Black + White)), White_perc = 100 * (White / (Black + White)))

select_year <- d$year[11]
d_year <- filter(d, year <= select_year) %>% select(year, Black, White, Black_perc, White_perc) 
p <- ggplot(data = d_year, aes(x=year, frame = year)) + 
  geom_point(aes(y = Black, colour = "Black",size=Black_perc, cumulative = TRUE)) +
  geom_point(aes(y = White, colour = "White",size=White_perc, cumulative = TRUE)) +
  scale_colour_manual(values=c("Black"="#6bffdd", "White"="#b869ff")) +
  scale_size(limits=c(0,100), range=c(1,7), guide = 'none') +
  xlim(1980,2020) + 
  scale_y_continuous(labels = scales::label_number_si(), limits=c(0,500000)) +
  labs(title="Racial Wealth Inequality in America", 
       subtitle="Data from the Urban Institute and the US Census showing the disparity in\nlifetime wealth between black and white Americans from 1983 to 2016.\nThe size of the points represents the relative wealth difference between races.\n",
       y="Median Lifetime Wealth (US Dollars)\n") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(panel.background = element_rect(fill = "#111111"),
        plot.background = element_rect(fill = "#111111"),
        legend.background = element_rect(fill = "#111111"),
        plot.title = element_text(colour = "#c51b8a", size=22, face="bold", hjust = 0.7),
        plot.subtitle = element_text(colour = "#c51b8a", size=12, hjust = 0.7),
        legend.title = element_blank(),
        legend.position=c(0.05,0.95),
        legend.justification=c(0,1),
        legend.key = element_rect(size = 5, fill = "#111111"),
        legend.text = element_text(colour="#c51b8a", size=12),
        axis.title.y=element_text(colour = "#c51b8a", size=12),
        axis.text.x=element_text(colour = "#c51b8a", size=12),
        axis.text.y=element_text(colour = "#c51b8a", size=12),
        axis.line.x = element_line(color="#c51b8a", size = 1),
        axis.line.y = element_line(color="#c51b8a", size = 1),
        plot.margin=unit(c(1,1.5,1,1),"cm"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank()
  )
p
ggsave(p, filename = "09022021.jpg", height=6, width=6, unit="in")

anim <- p +
  transition_states(year, transition_length = 4, state_length = 1) +
  enter_fade() +
  exit_fade() + shadow_mark()
animate(anim, nframes = 22, height = 6, width = 6, units = "in", res=150)
anim_save("09022021.gif", animation = last_animation())
