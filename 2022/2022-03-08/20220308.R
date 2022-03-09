library(tidyverse)
library(usefunc)
library(countrycode)
library(circlize)
library(showtext)
library(Polychrome)
library(cowplot)
library(ggplotify)
library(grid)

# get data
tuesdata <- tidytuesdayR::tt_load('2022-03-08')
erasmus <- tuesdata$erasmus

# load fonts
font_add_google(name = "Bodoni Moda", family = "Bodoni MT")
showtext_auto()

# get list of all countries either receive or send
countries <- erasmus %>% 
  select(sending_country_code, receiving_country_code) %>% 
  unique()
df_c <- data.frame(country_c = unique(c(countries$sending_country_code, 
                                        countries$receiving_country_code)))
# get continent
df_c$continent <- countrycode(sourcevar = df_c[, "country_c"],
                              origin = "iso2c",
                              destination = "continent")
df_c$continent[which(df_c$country_c %in% c("EL", "UK", "XK"))] <- c("Europe", "Europe", "Europe")

# arrange by continent
df_sort <- df_c %>% 
  as_tibble() %>% 
  arrange(continent, country_c)

# prep data
plot_data_asia <- erasmus %>% 
  select(sending_country_code, receiving_country_code) %>% 
  group_by(sending_country_code, receiving_country_code) %>% 
  summarise(n = n()) %>% 
  left_join(df_c, by = c("sending_country_code" = "country_c")) %>% 
  rename(sending_continent = continent) %>% 
  left_join(df_c, by = c("receiving_country_code" = "country_c")) %>% 
  rename(receiving_continent = continent) %>% 
  filter(.data$sending_country_code != receiving_country_code, 
         sending_continent == "Asia") %>% 
  arrange(receiving_continent) %>% 
  mutate(from = factor(sending_country_code, levels = unique(sending_country_code)), 
         to = factor(receiving_country_code, levels = unique(receiving_country_code))) %>% 
  ungroup() %>% 
  rename(value = n) %>% 
  select(from, to, value)
  
# choose colours
length(unique(c(plot_data_asia$from, plot_data_asia$to))) #36
data(palette36)

# make chord diagram
chordDiagram(plot_data_asia, grid.col = palette36)

# add ggplot2 themes
p <- recordPlot()
as.ggplot(ggdraw(p)) +
  labs(title="Erasmus: Where do students from Asian countries go?",
       subtitle="jdsjadk",
       caption="N. Rennie | Data: Data.Europa")+
  theme(panel.background = element_rect(fill = "gray97", colour="gray97"),
        plot.background = element_rect(fill = "gray97", colour="gray97"),
        legend.position="none",
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("20220308.jpg", height=7, width=7, unit = "in")

