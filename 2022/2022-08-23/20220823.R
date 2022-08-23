library(tidyverse)
library(showtext)

# load data
chips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-23/chips.csv')

# load fonts
font_add_google("Ubuntu", family = "ubuntu")
showtext_auto()

# prep data
plot_data <- chips %>% 
  drop_na()

# define legend
legend_bubbles <- data.frame(label = c("5000", "10000", "15000"),
                             size  = c(5000, 10000, 15000)) %>%
  mutate(radius = sqrt(size / pi))  

# plot
ggplot() + 
  geom_point(data = plot_data, 
             mapping = aes(y = factor(year), 
                           x = process_size_nm, 
                           size = transistors_million), 
             pch = 21) + 
  geom_point(data = legend_bubbles,
             mapping = aes(x = 150,
                           y = 3 + radius/77,
                           size = size),
             pch = 21) +
  geom_text(data = legend_bubbles,
            aes(x = 165,
                y = 3 + 2 * radius/77,
                label = label), 
            size = 3,
            family = "ubuntu") +
  geom_text(data = legend_bubbles,
            aes(x = 150,
                y = 2,
                label = "Transistors (millions)"), 
            size = 4,
            hjust = 0.5,
            family = "ubuntu") +
  geom_vline(xintercept = 200) +
  scale_size(range = c(3, 19), 
             breaks = c(5000, 10000, 15000)) +
  scale_y_discrete(limits = rev, expand = expansion(add = 1)) +
  scale_x_continuous(expand = expansion(add = 0), 
                     limits = c(0, 200)) +
  labs(y = "", 
       x = "Process size (nm)", 
       title = "CHIP Dataset", 
       caption = "N. Rennie | Data: chip-dataset.vercel.app") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, family = "ubuntu", size = 10), 
        plot.title = element_text(margin = margin(b = 10), family = "ubuntu", size = 20),
        axis.title.x = element_text(margin = margin(t = 10), family = "ubuntu"), 
        axis.title.y = element_text(family = "ubuntu"),
        axis.text = element_text(family = "ubuntu"),
        axis.line.y = element_line(color="black"),
        axis.line.y.right = element_line(color="black"),
        legend.position = "none",
        panel.grid.major.y = element_line(linetype = "dashed"),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5), unit = "cm"))

