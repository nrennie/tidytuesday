library(tidyverse)
library(showtext)
library(usefunc)
library(patchwork)
library(cowplot)
library(rcartocolor)

tuesdata <- tidytuesdayR::tt_load('2022-03-22')
babynames <- tuesdata$babynames

# load fonts
font_add_google(name = "Bungee Shade", family = "bungee")
font_add_google(name = "Dosis", family = "dosis")
showtext_auto()

# set colours
f_cols = c("#008080", "#329999", "#66b2b2",
           "#7fbfbf", "#99cccc", "#cce5e5")
m_cols = c("#4b0082", "#6e329b", "#9366b4",
           "#a57fc0", "#b799cd", "#dbcce6")

# number of unique names per year
num_names <- babynames %>% 
  group_by(year, sex) %>% 
  summarise(num = n())

# plot number of unique names per year
p1 <- ggplot(data = num_names, 
       mapping = aes(x = year, y = num, colour = sex)) + 
  geom_line() +
  geom_point(size = 1) +
  scale_colour_manual("", values = c(f_cols[1], m_cols[1]), labels = c("Female", "Male")) +
  scale_y_continuous(limits = c(0, 25000)) +
  coord_cartesian(expand = F) +
  labs(x = "", 
       y = "Number of unique baby names") +
  theme(legend.position = c(0.1, 0.9), 
        legend.title = element_blank(), 
        legend.text = element_text(family = "dosis", size = 14),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0), family = "dosis"), 
        axis.text = element_text(family = "dosis"),
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"), 
        panel.grid.major = element_line(colour = "#DEDEDE"), 
        panel.grid.minor = element_blank())
p1

###############################################################################################
###############################################################################################

# find 5 most popular names for F over all time
top_female_names <- babynames %>% 
  filter(sex == "F") %>% 
  group_by(name) %>% 
  summarise(total = sum(n)) %>% 
  slice_max(total, n = 5) %>% 
  mutate(rank = 1:5, 
         name = forcats::fct_reorder(name, -total)) %>% 
  pull(name)

plot_f_data <- babynames %>% 
  filter(sex == "F", 
         name %in% top_female_names) %>% 
  mutate(name = factor(name, levels = levels(top_female_names))) %>% 
  group_by(year, name) %>% 
  summarise(n = sum(n), 
            prop = sum(prop)) 

p2 <- ggplot(data = plot_f_data, 
       mapping = aes(x = year, y = n)) + 
  geom_area(aes(group = name, fill = name), colour = f_cols[1]) +
  facet_wrap(~name, nrow = 1) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_continuous(breaks = c(1900, 2000)) +
  scale_fill_manual(values = f_cols) +
  coord_cartesian(expand = F) +
  labs(x = "", y = "Number of babies per year\n") +
  theme(legend.position = "none", 
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"), 
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), family = "dosis"), 
        axis.text = element_text(family = "dosis"),
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"), 
        strip.text = element_text(family = "dosis", size = 14),
        strip.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"),
        panel.grid.major = element_line(colour = "#DEDEDE"), 
        panel.grid.minor = element_blank())
p2

###############################################################################################
###############################################################################################

# find 5 most popular names for M over all time
top_male_names <- babynames %>% 
  filter(sex == "M") %>% 
  group_by(name) %>% 
  summarise(total = sum(n)) %>% 
  slice_max(total, n = 5) %>% 
  mutate(rank = 1:5, 
         name = forcats::fct_reorder(name, -total)) %>% 
  pull(name)

plot_m_data <- babynames %>% 
  filter(sex == "M", 
         name %in% top_male_names) %>% 
  mutate(name = factor(name, levels = levels(top_male_names))) %>% 
  group_by(year, name) %>% 
  summarise(n = sum(n), 
            prop = sum(prop)) 

p3 <- ggplot(data = plot_m_data, 
             mapping = aes(x = year, y = n)) + 
  geom_area(aes(group = name, fill = name), colour = m_cols[1]) +
  facet_wrap(~name, nrow = 1) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_continuous(breaks = c(1900, 2000)) +
  scale_fill_manual(values = m_cols) +
  coord_cartesian(expand = F) +
  labs(x = "", y = "Number of babies per year\n") +
  theme(legend.position = "none", 
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"), 
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), family = "dosis"), 
        axis.text = element_text(family = "dosis"),
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"), 
        strip.text = element_text(family = "dosis", size = 14),
        strip.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"),
        panel.grid.major = element_line(colour = "#DEDEDE"), 
        panel.grid.minor = element_blank())
p3

###############################################################################################
###############################################################################################

# subtitle
st = str_wrap_break("Since 1880, there has consistently been more unique baby names per year in the USA for females than males, although the number has started to drop since the late-2000s. The most popular names for females and males are Mary and James, respectively, although their popularity is declining.\n\nN. Rennie | Data: {babynames}", 140)

# join plots
p <- p1 + (p2 / p3)  +
  plot_layout(widths = c(1, 2)) +
  plot_annotation(title = "BABY NAMES", 
                  subtitle = st) &
  theme(plot.title = element_text(margin = margin(20, 0, 10, 0), family = "bungee", size = 40), 
        plot.subtitle = element_text(margin = margin(10, 0, 10, 0), family = "dosis", size = 14),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"))
p

