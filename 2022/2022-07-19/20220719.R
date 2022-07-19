library(tidyverse)
library(showtext)
library(ggforce)
library(scales)
library(forcats)
library(usefunc)
library(patchwork)

# load fonts
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# read data
technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

# choose countries
countries <- c("GBR", "USA", "SWE", "BRA", "NZL", "VEN")

# prep data
raw_data <- technology %>% 
  filter(label == "% children who received a measles immunization") %>% 
  filter(year %in% c(1980, 2010)) %>% 
  rename(Year = year) %>% 
  filter(iso3c %in% countries) %>% 
  select(-c(group, category, variable, label)) %>% 
  mutate(no_value = 100 - value) %>% 
  pivot_longer(cols = c(value, no_value), names_to = "YN", values_to = "perc") %>% 
  pivot_wider(names_from = "Year", values_from = "perc") %>% 
  mutate(YN = factor(YN)) %>% 
  mutate(perc_1980 = `1980`/100, 
         perc_2010 = `2010`/100) %>% 
  select(-c(`1980`, `2010`)) %>% 
  group_by(iso3c) %>% 
  mutate(ymax_1980 = cumsum(perc_1980), 
         ymax_2010 = cumsum(perc_2010))

plot_data <- raw_data %>% 
  ungroup() %>% 
  mutate(ymin_1980 = c(rbind(rep(0, length(countries)), (slice_head(raw_data, n = -1) %>% pull(ymax_1980))))) %>% 
  mutate(ymin_2010 = c(rbind(rep(0, length(countries)), (slice_head(raw_data, n = -1) %>% pull(ymax_2010))))) %>% 
  group_by(iso3c) %>% 
  mutate_at(vars(starts_with("y", ignore.case = FALSE)), rescale, to = pi*c(-.5,.5), from = 0:1)

# Text Labels
text_df <- tibble(x = c(0.35, 0.85),
                  y = c(-0.1, -0.1), 
                  label = c(1980, 2010))

# Subtitle
st <- str_wrap_break("The inner bar represents the percentage of children who received a measles immunisation in 1980, whilst the outer bar represents the percentage in 2010. An increase in immunisation levels between 1980 and 2010 is seen across all countries.\n\nN. Rennie | Data: data.nber.org (10.3386/w15319)",
                     80)

# Full plot
p1 <- ggplot(data = plot_data) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1, start = ymin_2010, end = ymax_2010, fill= YN), 
               colour = "#949398") + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.2, r = 0.5, start = ymin_1980, end = ymax_1980, fill= YN), 
               colour = "#949398") + 
  geom_text(data = text_df, mapping = aes(x = x, y = y, label = label),
            family = "ubuntu", size = 10) +
  coord_fixed() + 
  scale_fill_manual(breaks = c("value", "no_value"),
                    labels = c("Immunised", "Not Immunised"),
                    values = c("#990c58", "#949398")) +
  labs(title = "Measles Vaccinations", 
       subtitle = st) +
  facet_wrap(~iso3c, nrow = 2) +
  theme_void() +
  theme(legend.position = "none", 
        legend.title = element_blank(), 
        strip.text = element_text(size = 30, family = "ubuntu"), 
        legend.text = element_text(size = 20, family = "ubuntu"), 
        plot.background = element_rect(fill = "#dedede", colour = "#dedede"), 
        panel.background = element_rect(fill = "#dedede", colour = "#dedede"), 
        plot.title = element_text(family = "ubuntu", 
                                  margin = margin(t = 10, b = 10), 
                                  face = "bold", 
                                  size = 38), 
        plot.subtitle = element_text(family = "ubuntu", 
                                  margin = margin(t = 10, b = 10), 
                                  size = 30, 
                                  lineheight = 0.5), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"))

# Legend plot
p2 <- ggplot(data = filter(plot_data, iso3c == "USA")) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1, 
                   start = ymin_2010, end = ymax_2010, fill= YN), 
               colour = "#949398") + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.2, r = 0.5,
                   start = ymin_1980, end = ymax_1980, fill= YN), 
               colour = "#949398") + 
  geom_text(data = text_df, mapping = aes(x = x, y = y, label = label), 
            family = "ubuntu", size = 10) +
  coord_fixed() + 
  scale_fill_manual(breaks = c("value", "no_value"),
                    labels = c("Immunised", "Not Immunised"),
                    values = c("#990c58", "#949398")) +
  labs(title = "How do I read this plot?") +
  facet_wrap(~iso3c) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        strip.text = element_text(size = 30, family = "ubuntu"), 
        legend.text = element_text(size = 20, family = "ubuntu"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.background = element_rect(fill = "transparent", colour = "transparent"), 
        plot.title = element_text(family = "ubuntu", 
                                  hjust = 0.5, 
                                  face = "italic",
                                  margin = margin(t = 10, b = 10), 
                                  size = 36), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"))

# Join together
p <- p1 + inset_element(p2, 0.5, 1.0, 1.1, 1.6) &
  theme(plot.background = element_rect(fill = "#dedede", colour = "#dedede"), 
        panel.background = element_rect(fill = "#dedede", colour = "#dedede"))

# Save
ggsave(p, filename = "2022/2022-07-19/20220719.png", width = 10, height = 7, unit = "in")

# Final annotations added with Inkscape
