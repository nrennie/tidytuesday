library(afrilearndata)
library(tidyverse)
library(sf)
library(rcartocolor)
library(showtext)

#add fonts
font_add_google(name = "Monoton", family = "monoton")

showtext_auto()

p <- ggplot(data=africountries) +
  geom_sf(aes(fill=gdp_md_est), colour=NA) +
  geom_sf(data=africontinent, colour="#faa476", fill="transparent") +
  facet_wrap(~ iso_a3, ncol=3, nrow=17) +
  labs(x="", y="", caption="N. Rennie | Data: afrilearndata", title="AFRICA") +
  scale_fill_carto_c("GDP", type = "diverging", palette = "SunsetDark", direction = -1, 
                     limits=c(0,500000), 
                     breaks=c(0,250000,500000),
                     labels=c("0", "250K", "500K"), 
                     guide = guide_colourbar(title.position = "top")) +
  theme(panel.background = element_rect(fill = "#001f3d", colour="#001f3d"),
        plot.background = element_rect(fill = "#001f3d", colour="#001f3d"),
        plot.margin = unit(c(0.8, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        strip.background =element_rect(fill="#001f3d"),
        strip.text = element_text(colour = '#faa476', family="sans", size=40),
        plot.title = element_text(colour = "#faa476", size=72, hjust = 0.5, family="monoton"),
        plot.caption = element_text(colour = "#faa476", size=30, hjust = 0.5, family="sans"),
        legend.text = element_text(colour = "#faa476", size=30, hjust = 0.5, family="sans"),
        legend.title = element_text(colour = "#faa476", size=30, hjust = 0.5, family="sans"),
        legend.background =element_rect(fill="#001f3d"),
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "bottom")

ggsave(p, filename="09112021.jpg", width=6,height=34,unit="in")
