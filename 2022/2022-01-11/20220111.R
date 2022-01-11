library(tidyverse)
library(poissoned)
library(showtext)

# read data
tuesdata <- tidytuesdayR::tt_load('2022-01-11')
colony <- tuesdata$colony
stressor <- tuesdata$stressor

# add fonts
font_add_google(name = "Source Code Pro", family = "source")
showtext_auto()

# prep data
plot_data <- colony %>% 
  filter(state %in% c("California", "Texas", "Florida", "New York", "Alabama", "Illinois"), 
         year != "6/") %>% 
  group_by(year, state) %>% 
  summarise(bees = sum(colony_n)) %>% 
  mutate(year = as.numeric(year))

# prep points for plot
bees_grid <- plot_data %>% 
  rowwise() %>% 
  mutate(
    t = sqrt(bees / 1000),
    pnts = list(poisson_disc(ncols = t, nrows = t, cell_size = 1 / t))
  ) %>% 
  ungroup() %>% 
  unnest(pnts)

# make plot
ggplot() +
  geom_tile(data = bees_grid, aes(0.5, 0.5, width = 1.07, height = 1.07), fill="#fecc27", color = "black", size = 0.5, stat = "unique") +
  geom_point(data = bees_grid, aes(x, y), size=0.3) +
  facet_grid(state ~ year, switch = "both") +
  labs(x = "", y = "", caption = "", 
       subtitle = "Bees are vital for the preservation of ecological balance and biodiversity\nin nature. Bee populations are rapidly declining around the world due to\nhabitat loss, pollution and the use of pesticides, among other factors. \n\nN.Rennie | Data: USDA\n", 
       title = "Bee colony losses in the United States\n") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"),
        strip.background =element_rect(fill="black"),
        strip.text = element_text(colour = '#fecc27', family="source", size=10), 
        plot.title = element_text(colour = '#fecc27', family="source", face = "bold", size=20),
        plot.subtitle = element_text(colour = '#fecc27', family="source", size=12),
        plot.caption = element_text(colour = '#fecc27', family="source", size=12), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


