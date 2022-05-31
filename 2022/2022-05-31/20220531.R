library(tidyverse)
library(showtext)

# get data
tuesdata <- tidytuesdayR::tt_load('2022-05-31')
poll <- tuesdata$poll
reputation <- tuesdata$reputation

# load fonts
font_add_google(name = "Poiret One", family = "poiret")
showtext_auto()

# prep data
plot_data <- reputation %>% 
  filter(industry == "Tech") %>% 
  rename("values" = "name") %>% 
  select(-c(rank, industry)) %>% 
  group_by(company) %>% 
  mutate(AVERAGE = round(mean(score), 1)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = values, 
              values_from = score) %>% 
  mutate(company = fct_reorder(company, AVERAGE)) %>% 
  pivot_longer(cols = -company) %>% 
  mutate(value2 = 100 - value) %>% 
  pivot_longer(cols = c(value, value2), names_to = "score_type", values_to = "score") %>% 
  mutate(score_type = factor(score_type, levels = c("value2", "value")))

# subtitle
st <- str_wrap("The Axios Harris Poll 100 is based on a survey of 33,096 Americans in a nationally representative sample conducted March 11-April 3, 2022. The two-step process starts fresh each year by surveying the public’s top-of-mind awareness of companies that either excel or falter in society. These 100 “most visible companies” are then ranked by a second group of Americans across the seven key dimensions of reputation to arrive at the ranking. Among tech companies, Samsung come out on top in almost all categories. Meanwhile, social media companies including Facebook and Twitter tend to perform more poorly.", 
               110)

# plot
p <- ggplot(plot_data) +
  geom_col(aes(x = score, y = company, fill = score_type)) +
  facet_wrap(~name, nrow = 2, ncol = 4) +
  labs(x = "", 
       y = "", 
       title = "2022 Axios-Harris Poll: Tech Companies", 
       subtitle = st,
       caption = "N. Rennie | Data: Axios and Harris Poll") +
  scale_fill_manual(values = c("value2" = "#66b2b2", "value" = "#006666")) +
  theme(plot.title = element_text(colour = "#004c4c", face = "bold", 
                                  size = 24, family = "poiret", 
                                  margin = margin(b = 10)), 
        plot.subtitle = element_text(size = 12, lineheight = 0.8,
                                     colour = "#004c4c", family = "poiret", 
                                     margin = margin(b = 10)), 
        axis.text = element_text(colour = "#004c4c", family = "poiret", 
                                 size = 10, lineheight = 0.4),
        strip.text = element_text(colour = "#004c4c", family = "poiret", 
                                  size = 12, lineheight = 0.4),
        plot.caption = element_text(colour = "#004c4c", family = "poiret", 
                                  size = 10, lineheight = 0.4),
        strip.background = element_rect(fill = "#b2d8d8", colour = "#b2d8d8"),
        legend.position = "none",
        panel.background = element_rect(fill = "#b2d8d8", colour = "#b2d8d8"),
        plot.background = element_rect(fill = "#b2d8d8", colour = "#b2d8d8"), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(1, 1.2, 0.5, 0.5), "cm"))
p











