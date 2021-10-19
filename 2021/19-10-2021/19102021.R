library(tidyverse)
library(cowplot)
library(showtext)
library(ragg) 
library(biscale) 
library(maps) 
library(sf) 

#read data
tuesdata <- tidytuesdayR::tt_load('2021-10-19')
pumpkins <- tuesdata$pumpkins

#add fonts
font_add_google(name = "Pacifico", family = "pacifico")
font_add_google(name = "Quicksand", family = "quicksand")

showtext_auto()

#prep data
plot_data <- pumpkins %>% 
  filter(country == "United States") %>% 
  mutate(year=factor(str_sub(id, start=1, end=4)), 
         type=str_sub(id, start=6, end=7),
         weight = as.numeric(weight_lbs), 
         ott_num = as.numeric(ott)) %>%
  filter(!is.na(weight), 
         !is.na(ott_num)) %>%
  select(state_prov, weight, ott_num, type) %>%
  group_by(state_prov, type) %>%
  summarise(weight=max(weight), 
            ott=max(ott_num))
plot_data$state_prov <- tolower(plot_data$state_prov) 
plot_data$type <- factor(plot_data$type, levels=c("F", "L", "P", "S", "W"), labels=c("Field Pumpkin", "Long Gourd", "Giant Pumpkin", "Giant Squash", "Giant Watermelon"))

w <- plot_data %>% 
  select(state_prov, type, weight) %>%
  pivot_wider(names_from = type, values_from = weight, values_fill = NA) %>%
  pivot_longer(cols=2:6) 

o <- plot_data %>% 
  select(state_prov, type, ott) %>%
  pivot_wider(names_from = type, values_from = ott, values_fill = NA) %>%
  pivot_longer(cols=2:6) 

p_dat <-   cbind(w, o$value)
colnames(p_dat) <- c("state_prov", "type", "weight", "ott")
p_dat %>% add_row()

nm <- tibble("state_prov"=rep("new mexico", 5), 
       "type"=unique(p_dat$type), 
       weight=rep(NA_real_,5),
       ott=rep(NA_real_,5))
p_dat1 <- bind_rows(p_dat, nm)
#prep map
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
names(states)[1] <- "state_prov" 
map_polygons <- merge(states, p_dat1, by="state_prov") 
us_classes <- bi_class(map_polygons, x=weight, y=ott, style="quantile", dim=3) %>% 
  mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))

#plot map
p <- ggplot() + 
  geom_sf(data=us_classes, mapping=aes(fill=bi_class), color="#6e422e", size=0.1, show.legend=FALSE) +
  bi_scale_fill(pal="Brown", dim=3, na.value="grey50") + 
  labs(title="Big Pumpkins\n",
       subtitle = "The Great Pumpkin Commonwealth's mission cultivates the hobby of growing\ngiant pumpkins throughout the world. Here, the maximum weight and over the\ntop inches are compared for different types of pumpkins in the United States.\nIt appears that the Giant Pumpkin is aptly named.\n",
       caption="N. Rennie | Data: BigPumpkins.com") + 
  facet_wrap(~type) +
  theme(plot.background = element_rect(fill = "#f1ebd2", colour="#f1ebd2"),
        panel.background = element_rect(fill = "#f1ebd2", colour="#f1ebd2"),
        strip.background =element_rect(fill="#f1ebd2"),
        strip.text = element_text(colour = '#6e422e', family="pacifico", size=12),
        plot.title = element_text(colour = "#6e422e", size=28, hjust = 0, family="pacifico"),
        plot.subtitle = element_text(colour = "#6e422e", size=18, hjust = 0, family="quicksand"),
        plot.caption = element_text(colour = "#6e422e", size=12, hjust = 0, family="quicksand"),
        legend.position="none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

#add legend
p_legend <- bi_legend(pal="Brown",
                        dim=3,
                        xlab="Weight (lbs)",
                        ylab="OTT (in)",
                        size=12) + 
  theme(plot.background = element_rect(fill = "#f1ebd2", colour="#f1ebd2"),
        panel.background = element_rect(fill = "#f1ebd2", colour="#f1ebd2"),
        axis.title = element_text(colour = "#6e422e", size=12, hjust = 0))
p_legend
q <- ggdraw() + 
  draw_plot(p, 0, 0, 1, 1) +
  draw_plot(p_legend, 0.74, 0.15, 0.2, 0.2, scale=1)
q


