library(tidyverse)
library(showtext)
library(usefunc)
library(scales)

tuesdata <- tidytuesdayR::tt_load('2022-03-29')
sports <- tuesdata$sports

# load fonts
font_add_google(name = "Black Ops One", family = "opsone")
font_add_google(name = "Dosis", family = "dosis")
showtext_auto()

# prep data
plot_data <- sports %>% 
  select(exp_men, exp_women, sports) %>%
  drop_na() %>% 
  filter(exp_men != 0, 
         exp_women != 0) %>% 
  group_by(sports) %>% 
  summarise(mean_exp_men = mean(exp_men), 
         mean_exp_women = mean(exp_women)) %>% 
  mutate(mean_exp_diff = mean_exp_men - mean_exp_women) %>% 
  select(sports, mean_exp_diff) %>% 
  mutate(sports = fct_reorder(sports, mean_exp_diff), 
         less = as.factor(mean_exp_diff < 0))

# define subtitle
st <- str_wrap_break("\nAcross all sports, expenditure is fairly equal between male and female teams, with 14 of the 31 listed college sports spending more on male teams on average. For most sports the difference in expenditure for male and female teams is small. Notable excpetions are basketball and ice hockey which spend almost $400,000 more per year on male than female teams. Gymnastics does the opposite.\n\nN. Rennie | Data: Equity in Athletics Data Analysis", 100)

# plot
ggplot(data = plot_data, 
       mapping = aes(x = mean_exp_diff, y = sports, colour = less)) +
  geom_point(size = 3) +
  geom_segment(aes(yend = sports, xend = 0), size = 1) +
  # add arrows
  annotate("segment", y = 1, yend = 17, x = 150000, xend = 150000, colour = "#4b0082",
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("segment", y = 18, yend = 31, x = -150000, xend = -150000, colour = "#008080",
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  # add text
  annotate("text", x = 275000, y = 9, label = "Higher female expenditure", 
           colour = "#4b0082", family = "dosis") +
  annotate("text", x = -275000, y = 24.5, label = "Higher male expenditure", 
           colour = "#008080", family = "dosis") +
  labs(x = "Average difference in annual expenditure\nfor male and female sports (US $)", 
       y = "",
       title = "Expenditure in College Sports", 
       subtitle = st) +
  scale_x_continuous(labels = unit_format(unit = "K", scale = 1e-3, sep = ""), 
                     limits = c(-600000, 600000), 
                     expand = c(0, 0)) +
  scale_colour_manual("", values = c("#008080", "#4b0082")) +
  coord_flip() +
  plot_theme(main_font = "dosis", 
             title_font = "opsone") +
  theme(plot.margin = unit(c(0.8, 0.8, 0.5, 0.8), "cm"), 
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(lineheight = 1, margin = margin(t = 15, b = 20)),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none")



