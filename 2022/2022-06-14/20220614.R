library(tidyverse)
library(lubridate)
library(usefunc)
library(geofacet)
library(ggstream)
library(forcats)
library(showtext)

# load fonts
font_add_google(name = "Bowlby One SC", family = "bowlby")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# load data
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
drought

# prep data
plot_data <- drought %>% 
  select(DATE, state, D0, D1, D2, D3, D4) %>% 
  # fix date
  mutate(date = parse_number(DATE), 
         date = ymd(date)) %>% 
  mutate(year = year(date)) %>% 
  select(-c(DATE, date)) %>% 
  # fix state names
  mutate(state = str_replace(state, pattern = "-", replacement = " "), 
         state = str_to_title(state)) %>% 
  mutate(state_abb = US_name_to_abb(state)) %>% 
  select(-state) %>% 
  pivot_longer(D0:D4, values_to = "perc", names_to = "level") %>% 
  # average per year
  mutate(decade = floor(year/10)*10) %>% 
  group_by(decade, state_abb, level) %>% 
  mutate(mean_perc = mean(perc)) %>% 
  select(-c(year, perc)) %>% 
  # levels as factor
  mutate(level = factor(level), 
         level = fct_rev(level)) %>% 
  ungroup() %>% 
  complete(level, decade, state_abb) %>% 
  distinct()

# plot viewer
dev.new(width=12,height=8,unit="in", noRStudioGD = TRUE)

# plot
ggplot(data = plot_data, 
       mapping = aes(x = decade, y = mean_perc, fill = level)) +
  geom_stream(type = "ridge", extra_span = .1) + 
  geom_text(data = filter(plot_data, decade == 1960, level == "D0"), 
            aes(x = 1960, y = 35, label = state_abb), 
            family = "bowlby", size = 5, hjust = 0.5, 
            colour = alpha("#808080", 0.5)) +
  facet_geo(~state_abb, grid = "us_state_contiguous_grid1") +
  scale_fill_brewer(palette = "YlOrRd", direction = -1, 
                    breaks = c("D0", "D1", "D2", "D3", "D4"), 
                    labels = str_wrap(c("Abnormally dry", 
                               "Moderate drought", 
                               "Severe drought", 
                               "Extreme drought", 
                               "Exceptional drought"), 10)) +
  scale_x_continuous(breaks = c(1920, 1980)) +
  labs(x = "", 
       y = "", 
       title = "US Droughts", 
       caption = "N. Rennie | Data: National Integrated Drought Information System") +
  coord_cartesian(expand = FALSE) +
  theme(plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
        legend.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
        legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        strip.background = element_blank(), 
        strip.text = element_blank(),
        plot.title = element_text(family = "bowlby", size = 30, hjust = 0.5, 
                                  colour = "#808080", margin = margin(t = 10, b = 10)), 
        legend.text = element_text(family = "ubuntu", size = 10, hjust = 0.5, 
                                  colour = "#808080"), 
        plot.caption = element_text(family = "ubuntu", size = 10, hjust = 0.5, 
                                   colour = "#808080", margin = margin(t = 10, b = 10)), 
        axis.text.x = element_text(family = "ubuntu", size = 10, hjust = 0.5, 
                                               colour = "#808080")
        )

