library(tidyverse)
library(showtext)
library(cowplot)

# load data
df <- readr::read_csv("2022/data/07.csv")

# load fonts
font_add_google(name = "Space Mono", family = "space")
font_add_google(name = "Cormorant Garamond", family = "corgar")
showtext_auto()

# prep data
plot_data <- df %>% 
  pivot_longer(cols = 3:5, names_to = "status", values_to = "perc") %>% 
  mutate(new_perc = perc* c(1, -1)[(as.numeric(Gender == "Male")+1)]) %>%  
  unite(col = "new_status", c(Gender, status), sep="-") %>% 
  mutate(new_status_f = factor(new_status, 
                               levels = c("Male-Widowed", "Male-Married", "Male-Single", 
                                          "Female-Widowed", "Female-Married", "Female-Single")))
# text df
text_df_left = tibble(x = c(-92, -60, -35), 
                      y = c(8.5, 5.5, 2), 
                      label = c("WIDOWED", "MARRIED", "SINGLE"))

text_df_right = tibble(x = c(92, 60, 35), 
                       y = c(8.5, 5.5, 2), 
                       label = c("WIDOWED", "MARRIED", "SINGLE"))

# plot
p <- ggplot() +
  geom_col(data = plot_data, mapping = aes(x = new_perc, y = Group, fill = new_status_f), width = 1, alpha = 0.94) +
  geom_text(data = text_df_left, mapping = aes(x = x, y = y, label = label), family = "roboto", angle = 60, size = 6) +
  geom_text(data = text_df_right, mapping = aes(x = x, y = y, label = label), family = "roboto", angle = -60, size = 6) +
  coord_cartesian(expand = F) +
  scale_x_continuous(breaks = c(seq(-100, 100, 10)), labels = c(seq(100, 10, -10), "", seq(10, 100, 10))) +
  labs(title = "Conjugal condition of American Negroes according to age periods.", 
       subtitle = "Condition conjugale des Negres Americains au point de vue de l'age.",
       x = "PER CENTS.", 
       y = "AGES", 
       tag = "Done by Atlanta University") +
  scale_fill_manual(values = c("Male-Widowed" = "#2e8059", "Male-Married"="#ca183b", "Male-Single" = "#064da1", 
                               "Female-Single" = "#064da1", "Female-Married"="#ca183b", "Female-Widowed"="#2e8059")) +
  theme(plot.background = element_rect(fill = "#dfd3c2", colour="#dfd3c2"),
        panel.background = element_rect(fill = "#dfd3c2", colour="#dfd3c2"),
        legend.position = "none", 
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "black"),
        plot.margin = unit(c(1, 1.1, 1, 0.1), "cm"),
        plot.title = element_text(colour = "black", size=20, face = "bold", hjust = 0.5, family="corgar", 
                                  margin = margin(0, 0, 10, 0)), 
        plot.subtitle = element_text(colour = "black", size=14, hjust = 0.5, family="corgar", 
                                     margin = margin(0, 0, 30, 0)), 
        plot.tag = element_text(colour = "black", size=12, hjust = 0.5, family="corgar"), 
        plot.tag.position = c(0.56, 0.9),
        axis.text.x = element_text(colour = "black", size=14, hjust = 0.5, family="roboto"), 
        axis.text.y = element_text(colour = "black", size=14, vjust = 0, family="roboto"), 
        axis.title.x = element_text(colour = "black", size=14, family="roboto"),
        axis.title.y = element_text(colour = "black", size=14, angle = 0, hjust = 110, vjust = 0.98, family="roboto"))
p

# add labels
q <- ggdraw() +
  draw_plot(p) +
  draw_label("MALES", x = 0.35, y = 0.80, colour = "black", size=15, fontfamily="roboto") +
  draw_label("FEMALES", x = 0.67, y = 0.80, colour = "black", size=15, fontfamily="roboto") + 
  draw_label("0-15", x = 0.93, y = 0.16, colour = "black", size=14, fontfamily="roboto", hjust = 0.5) +
  draw_label("15-20", x = 0.93, y = 0.23, colour = "black", size=14, fontfamily="roboto", hjust = 0.5) +
  draw_label("20-25", x = 0.93, y = 0.305, colour = "black", size=14, fontfamily="roboto", hjust = 0.5) +
  draw_label("25-30", x = 0.93, y = 0.38, colour = "black", size=14, fontfamily="roboto", hjust = 0.5) +
  draw_label("30-35", x = 0.93, y = 0.455, colour = "black", size=14, fontfamily="roboto", hjust = 0.5) +
  draw_label("35-45", x = 0.93, y = 0.53, colour = "black", size=14, fontfamily="roboto", hjust = 0.5) +
  draw_label("45-55", x = 0.93, y = 0.60, colour = "black", size=14, fontfamily="roboto", hjust = 0.5) +
  draw_label("55-65", x = 0.93, y = 0.675, colour = "black", size=14, fontfamily="roboto", hjust = 0.5) +
  draw_label("Over 65", x = 0.93, y = 0.76, colour = "black", size=14, fontfamily="roboto", hjust = 0.5) +
  draw_label("AGES", x = 0.92, y = 0.775, colour = "black", size=14, fontfamily="roboto", hjust = 0.5) 
q

ggsave(q, filename = "2022/2022-02-15/20220215.jpg", height = 5, width = 4, unit = "in")
