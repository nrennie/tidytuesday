library(tidyverse)
library(showtext)
library(magrittr)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load('2020-12-08')
women <- tuesdata$women

# add fonts
font_add_google(name = "Bungee", family = "bungee")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# prep data
plot_data <- women %>%
  select(name, img, category, country, role) %>%
  mutate(theta = seq(0, 2 * pi, length.out = 100),
         x = 10 * cos(theta),
         y = 10 * sin(theta),
         labx = 12 * cos(theta),
         laby = 12 * sin(theta),
         angle = 360*(theta/(2*pi)))

# define edges
Creativity <- plot_data %>%
  filter(category == "Creativity") %>%
  select(x, y) %>%
  mutate(id = row_number())
Creativity_edges <- tibble(expand.grid(id1 = Creativity$id, id2 = Creativity$id)) %>%
  inner_join(Creativity, by = c("id1" = "id")) %>%
  inner_join(Creativity, by = c("id2" = "id")) %>%
  select(-c(id1, id2)) %>%
  set_colnames(c("x", "y", "xend", "yend"))

Identity <- plot_data %>%
  filter(category == "Identity") %>%
  select(x, y) %>%
  mutate(id = row_number())
Identity_edges <- tibble(expand.grid(id1 = Identity$id, id2 = Identity$id)) %>%
  inner_join(Identity, by = c("id1" = "id")) %>%
  inner_join(Identity, by = c("id2" = "id")) %>%
  select(-c(id1, id2)) %>%
  set_colnames(c("x", "y", "xend", "yend"))

Knowledge <- plot_data %>%
  filter(category == "Knowledge") %>%
  select(x, y) %>%
  mutate(id = row_number())
Knowledge_edges <- tibble(expand.grid(id1 = Knowledge$id, id2 = Knowledge$id)) %>%
  inner_join(Knowledge, by = c("id1" = "id")) %>%
  inner_join(Knowledge, by = c("id2" = "id")) %>%
  select(-c(id1, id2)) %>%
  set_colnames(c("x", "y", "xend", "yend"))

Leadership <- plot_data %>%
  filter(category == "Leadership") %>%
  select(x, y) %>%
  mutate(id = row_number())
Leadership_edges <- tibble(expand.grid(id1 = Leadership$id, id2 = Leadership$id)) %>%
  inner_join(Leadership, by = c("id1" = "id")) %>%
  inner_join(Leadership, by = c("id2" = "id")) %>%
  select(-c(id1, id2)) %>%
  set_colnames(c("x", "y", "xend", "yend"))

# plot
p1 <- ggplot() +
  geom_segment(data = Creativity_edges,
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               colour = alpha("#884c94", 0.5), size = 0.05) +
  geom_point(data = plot_data,
             mapping = aes(x = x, y = y, colour = category),
             size = 0.08) +
  geom_text(data = plot_data,
            mapping = aes(x = labx, y = laby, label = name, angle = angle, colour = category),
            family = "ubuntu", hjust = 0, size = 2) +
  scale_colour_manual("", values = c("white", "#884c94", "#26aa83", "#4a75b0", "#ff3377")) +
  labs(subtitle = "Creativity") +
  xlim(-20, 20) +
  ylim(-20, 20) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        legend.text = element_text(family = "ubuntu", hjust = 0.5, size = 12, color = "white"),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0.5, size = 18, color = "white"),
        plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p1

p2 <- ggplot() +
  geom_segment(data = Identity_edges,
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               colour = alpha("#26aa83", 0.5), size = 0.05) +
  geom_point(data = plot_data,
             mapping = aes(x = x, y = y, colour = category),
             size = 0.08) +
  geom_text(data = plot_data,
            mapping = aes(x = labx, y = laby, label = name, angle = angle, colour = category),
            family = "ubuntu", hjust = 0, size = 2) +
  scale_colour_manual("", values = c("white", "#884c94", "#26aa83", "#4a75b0", "#ff3377")) +
  labs(subtitle = "Identity") +
  xlim(-20, 20) +
  ylim(-20, 20) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        legend.text = element_text(family = "ubuntu", hjust = 0.5, size = 12, color = "white"),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0.5, size = 18, color = "white"),
        plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p2

p3 <- ggplot() +
  geom_segment(data = Knowledge_edges,
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               colour = alpha("#4a75b0", 0.5), size = 0.05) +
  geom_point(data = plot_data,
             mapping = aes(x = x, y = y, colour = category),
             size = 0.08) +
  geom_text(data = plot_data,
            mapping = aes(x = labx, y = laby, label = name, angle = angle, colour = category),
            family = "ubuntu", hjust = 0, size = 2) +
  scale_colour_manual("", values = c("white", "#884c94", "#26aa83", "#4a75b0", "#ff3377")) +
  labs(subtitle = "Knowledge") +
  xlim(-20, 20) +
  ylim(-20, 20) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        legend.text = element_text(family = "ubuntu", hjust = 0.5, size = 12, color = "white"),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0.5, size = 18, color = "white"),
        plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p3

p4 <- ggplot() +
  geom_segment(data = Leadership_edges,
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               colour = alpha("#ff3377", 0.5), size = 0.05) +
  geom_point(data = plot_data,
             mapping = aes(x = x, y = y, colour = category),
             size = 0.08) +
  geom_text(data = plot_data,
            mapping = aes(x = labx, y = laby, label = name, angle = angle, colour = category),
            family = "ubuntu", hjust = 0, size = 2) +
  scale_colour_manual("", values = c("white", "#884c94", "#26aa83", "#4a75b0", "#ff3377")) +
  labs(subtitle = "Leadership") +
  xlim(-20, 20) +
  ylim(-20, 20) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        legend.text = element_text(family = "ubuntu", hjust = 0.5, size = 12, color = "white"),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0.5, size = 18, color = "white"),
        plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p4

# join plot
p <- p1 + p2 + p3 + p4 +
  plot_annotation(title = "BBC 100 Women 2020",
                  caption = "N. Rennie | Data: BBC | #30DayChartChallenge") &
  theme(plot.title = element_text(family = "ubuntu", hjust = 0.5,
                                  face = "bold", size = 32, color = "white"),
        plot.caption = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 12, color = "white"),
        plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"))
p

# save
ggsave(p, filename = "2022/viz/day_17.jpg", unit = "in", height = 5, width = 4)

