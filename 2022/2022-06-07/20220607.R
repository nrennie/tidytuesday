library(tidyverse)
library(showtext)
library(usefunc)
library(patchwork)

# load fonts
font_add_google("Ubuntu", family = "ubuntu")
showtext_auto()

# get data
pride_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv')

# prep data
plot_data <- pride_aggregates %>% 
  filter(Company != "Grand Total") %>% 
  arrange(-`Total Contributed`) %>% 
  slice_head(n = 6) %>% 
  mutate(Company = str_extract(Company, pattern = "[^(]+")) %>% 
  mutate(Company = str_trim(Company)) %>% 
  select(-`# of States Where Contributions Made`) 
plot_data2 <- plot_data %>% 
  mutate(Company = factor(Company, levels = plot_data$Company)) %>% 
  rename("Contribution" = `Total Contributed`, 
         "Politicians" = `# of Politicians Contributed to`) %>% 
  pivot_longer(-Company) %>% 
  mutate(x = ifelse(name == "Contribution", 0, 1))

# plot
p1 <- ggplot() +
  geom_area(data = plot_data2, 
            mapping = aes(x = x, y = value, fill = Company), 
            position = "fill") +
  geom_text(data = data.frame(x = rep(0.03, 6), 
                              y = c(0.8, 0.44, 0.26, 0.18, 0.1, 0.04), 
                              label = plot_data$Company), 
            mapping = aes(x = x, y = y, label = label), 
            hjust = 0,
            family = "ubuntu",
            colour = "white") +
  geom_text(data = data.frame(x = c(-0.05, 1.05), 
                              y = c(0.5, 0.5), 
                              label = c("Total contributed to anti-LBGTQ+ politicians\n(min: $ 79,550, max: $ 601,500)\n", 
                                        "Number of politicians contributed to\n(min: 7, max: 67)\n"), 
                              angle = c(90, -90), 
                              family = "ubuntu"), 
            mapping = aes(x = x, y = y, label = label, angle = angle)) +
  xlim(-0.1, 1.1) +
  scale_fill_manual(values = c("#E50000", "#FF8D00", "#FFEE00", 
                               "#028121", "#004CFF", "#770088")) +
  theme(panel.background = element_rect(colour = "white", fill = "white"), 
        plot.background = element_rect(colour = "white", fill = "white"), 
        axis.text = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "none", 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"), 
        plot.title = element_blank())

# subtitle
st <- str_wrap_break("June is Pride month, and many companies start selling rainbow-coloured merchandise to present themselves as LGBTQ+ allies. But many companies who sponsor Pride events, also contribute to anti-LGBTQ+ campaigns and politicians. The six largest donors alone contributed over $1.2 million. Toyota are the largest donors to anti-LGBTQ+ campaigns and, in contrast to other companies, these donations are concentrated to a small number of politicians. \n\n N. Rennie | Data: Data for Progress", 
                     45)
# title plot
p2 <- ggplot() +
  geom_text(data = data.frame(x = 0, y = 1, label = "Pride Donations"), 
            mapping = aes(x = x, y = y, label = label), 
            hjust = 0, family = "ubuntu", size = 8, fontface = "bold") +
  geom_text(data = data.frame(x = 0, y = 0.5, label = st), 
            mapping = aes(x = x, y = y, label = label), 
            hjust = 0, family = "ubuntu") +
  xlim(-0.3, 3) +
  ylim(0, 1.1) +
  theme_void()

# join
p2 + p1 +
  plot_layout(ncol = 2, widths = c(1, 1.6))
