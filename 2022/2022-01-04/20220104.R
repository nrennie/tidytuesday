library(geomtextpath)
library(cowplot)

txt <- readLines("2022/2022-01-04/thesis.txt")
theta <- seq(0, 30 * pi, by=0.01)
r <- 0.5 + 0.5 * theta
x = r * cos(theta)
y = r * sin(theta)
plot_data <- data.frame(x    = r * cos(theta), 
                        y    = r * sin(theta),
                        text = txt)

p <- ggplot(plot_data, aes(x, y, label = text)) +
  geom_textpath(size = 4, vjust = 2, text_only = TRUE) +
  coord_equal() + 
  labs(caption = "N. Rennie | Data: Rennie, Nicola (2021). Detecting demand outliers in transport systems. PhD thesis.", 
       title="Detecting demand outliers in transport systems") +
  theme_void() +
  theme(plot.title = element_text(colour = "black", size=24, family="serif", face="bold", hjust=0.5, vjust=0, margin = margin(15,2,30,2)),
        plot.caption = element_text(colour = "black", size=10, family="serif", hjust=0.5, margin = margin(2,2,2,2)), 
        plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank()
  )
p

q <- ggdraw() + 
  draw_plot(p) +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"))
q


