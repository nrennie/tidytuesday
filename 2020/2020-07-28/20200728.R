library(tidyverse)
library(ggdist)
library(showtext)
library(rcartocolor)
library(usefunc)
library(palmerpenguins)

# add fonts
font_add_google(name = "Bungee", family = "bungee")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# prep data
plot_data <- penguins %>%
  select(species, body_mass_g) %>%
  drop_na()

# plot
p <- ggplot(data = plot_data,
            mapping = aes(x = species, y = body_mass_g, fill = species)) +
  stat_gradientinterval(position = "dodge",
                        colour = NA,
                        width = 1) +
  stat_halfeye(adjust = .3,
               width = .3,
               .width = 0,
               justification = -.3,
               point_colour = 'NA',
               slab_fill=NA,
               slab_colour='#3e2c12',
               slab_size=0.4) +
  geom_boxplot(width = .15,
               outlier.shape = NA,
               fill='#fafafa') +
  stat_dots(
    side = "left",
    dotsize = .8,
    justification = 1.15,
    binwidth = 75,
    colour='#3e2c12'
  ) +
  coord_cartesian(xlim = c(1, NA),
                  ylim = c(2000, 7000),
                  clip = "off") +
  scale_fill_manual(values = c("#1a8b71", "#bdd73d", "#0b4358")) +
  guides(fill="none", alpha='none') +
  labs(x = "", y = "Body mass (g)",
       title = "Palmer Penguins",
       subtitle = str_wrap_break("Gentoo penguins have significantly higher body weights than Adelie or Chinstrap penguins, with an average mass of around 5 kg. In contrast, the average Adelie or Chinstrap penguin weighs sround 3.7 kg.\n\nN. Rennie | Data: {palmerpenguins}", 90)) +
  theme_minimal() +
  theme(axis.title.y = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black",
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black",
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black"),
        plot.title = element_text(family = "bungee", hjust = 0, size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0, size = 12, color = "black"),
        plot.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        legend.position = "none",
        axis.ticks = element_blank()
  )

p
