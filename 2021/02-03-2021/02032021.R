library(tidyverse)
library(ggimage)
library(cowplot)
library(magick)
library(ggforce)

tuesdata <- tidytuesdayR::tt_load('2021-03-02')
youtube <- tuesdata$youtube

youtube2020 <- filter(youtube, year == 2020)

#find coordinate points of circle
z <- 0:8
r <- 1
x <- r*cos(40*z + 20)
y <- r*sin(40*z + 20)
#plot logos as points at x and y
d <- data.frame(x = x,
                y = y,
                image = c("logo/doritos.png",
                          "logo/budweiser.png",
                          "logo/pepsi.png",
                          "logo/toyota.png",
                          "logo/nfl.png",
                          "logo/cola.png",
                          "logo/budlight.png",
                          "logo/kia.png",
                          "logo/hyundai.png")
)
d$y[2] <- 0.05
d$x[2] <- -0.85
d$y[3] <- -0.65
d$x[3] <- 0.6
d$y[5] <- -0.65
d$y[6] <- -0.2
d$y[7] <- 0.64
d$x[6] <- 0.75
p1 <- ggplot(d, aes(x, y)) + 
  xlim(-1.2, 1.2) + ylim(-2.1, 1.8) +
  geom_point(data=data.frame(p=0, q=0), aes(x=p, y=q),colour="purple4", size = 145, alpha=.2) +
  geom_ellipse(aes(x0 = 0.15, y0 = 0.4, a = 1.15, b = 0.70, angle = (140*pi / 180)), fill="springgreen4", colour="springgreen4", alpha=0.02) +
  geom_ellipse(aes(x0 = -0.75, y0 = -0.4, a = 0.8, b = 0.4, angle = (105*pi / 180)), fill="deeppink3", colour="deeppink3", alpha=0.02) +
  geom_ellipse(aes(x0 = -0.28, y0 = 0.58, a = 1.05, b = 0.57, angle = (37*pi / 180)), fill="dodgerblue2", colour="dodgerblue2", alpha=0.02) +
  geom_ellipse(aes(x0 = 0.35, y0 = 0.55, a = 1.10, b = 0.60, angle = (120*pi / 180)), fill="#FFA500", colour="#FFA500", alpha=0.02) +
  geom_image(aes(image=image),size=c(0.2,0.08,0.2,0.2,0.2,0.15,0.1,0.1,0.1), by="height") +
  #legend
  geom_point(data=data.frame(p=-1.0, q=-1.9), aes(x=p, y=q),colour="purple4", size = 9, alpha=.2) +
  geom_point(data=data.frame(p=-0.5, q=-1.9), aes(x=p, y=q),colour="springgreen4", size = 9, alpha=.2) +
  geom_point(data=data.frame(p=0, q=-1.9), aes(x=p, y=q),colour="deeppink3", size = 9, alpha=.2) +
  geom_point(data=data.frame(p=0.5, q=-1.9), aes(x=p, y=q),colour="dodgerblue2", size = 9, alpha=.2) +
  geom_point(data=data.frame(p=1.0, q=-1.9), aes(x=p, y=q),colour="#FFA500", size = 9, alpha=.2) +
  #add labels
  annotate("text", x = -1.0, y = -2.1, label = "Celebrity", hjust = 0.5, family="Berlin Sans FB") +
  annotate("text", x = -0.5, y = -2.1, label = "Funny", hjust = 0.5, family="Berlin Sans FB") +
  annotate("text", x = 0, y = -2.1, label = "Patriotic", hjust = 0.5, family="Berlin Sans FB") +
  annotate("text", x = 0.5, y = -2.1, label = "Danger", hjust = 0.5, family="Berlin Sans FB") +
  annotate("text", x = 1.0, y = -2.1, label = "Animals", hjust = 0.5, family="Berlin Sans FB") +
  theme(panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "grey85"),
        legend.background = element_rect(fill = "grey85"),
        legend.position="none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank()
  )


p1


############################################# BAR CHART OF LLIKES / DISLIKES ####################################################

youtube2020likes <- youtube2020 %>% select(brand, like_count, dislike_count)
youtube2020likes <- pivot_longer(youtube2020likes, cols=c(like_count, dislike_count), values_to = "count")

p2 <- ggplot(data=youtube2020likes, aes(x=brand, y=count, fill=name)) +
  labs(y="Numbee of YouTube Likes / Dislikes\n", x="") + 
  geom_bar(width=0.5, stat="identity", position=position_dodge(width=0.5)) +
  scale_fill_manual("", values=c("dislike_count"="red3", "like_count"="green4"), labels = c("Dislikes", "Likes")) +
  scale_y_continuous(trans='sqrt', breaks=c(5000,20000,50000,100000,175000), limits=c(0,176000), labels = scales::label_number_si()) +
  coord_cartesian(expand=F) +
  theme(panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "grey85"),
        legend.background = element_rect(fill = "grey85"),
        plot.title = element_text(colour = "red3", size=9, face="bold", hjust = 0.5, family="Berlin Sans FB"),
        plot.subtitle = element_text(colour = "red3", size=9, hjust = 0.5, family="Berlin Sans FB"),
        legend.title = element_blank(),
        legend.position="top",
        legend.key = element_rect(size = 1.2, colour = "grey85"),
        legend.spacing.x = unit(0.5,"cm"),
        plot.margin = unit(c(0.5, 0.8, 0.8, 0.5), "cm"), #top, right, bottom, left
        legend.text = element_text(colour="black", size=9, family="Berlin Sans FB"),
        axis.title.y= element_text(colour="black", size=8, family="Berlin Sans FB"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour = "black", size=8, family="Berlin Sans FB"),
        axis.line = element_line(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size=.1, color="black"), 
        panel.grid.minor = element_blank()
  )
p2

#dev.new(width=8,height=3,unit="in", noRStudioGD = TRUE)

p_b <- ggdraw() +
  draw_plot(p2) +
  draw_image("logo/budlight.png", scale = 0.15, x=-0.33, y=-0.42) +
  draw_image("logo/budweiser.png", scale = 0.1, x=-0.23, y=-0.42) +
  draw_image("logo/cola.png", scale = 0.13, x=-0.14, y=-0.42) +
  draw_image("logo/doritos.png", scale = 0.2, x=-0.05, y=-0.42) +
  draw_image("logo/hyundai.png", scale = 0.12, x=0.05, y=-0.42) +
  draw_image("logo/kia.png", scale = 0.12, x=0.14, y=-0.42) +
  draw_image("logo/nfl.png", scale = 0.15, x=0.25, y=-0.42) +
  draw_image("logo/pepsi.png", scale = 0.15, x=0.34, y=-0.42) +
  draw_image("logo/toyota.png", scale = 0.17, x=0.43, y=-0.42)
p_b


###############################################################################################################################


p <- plot_grid(p1, p_b, ncol=1, rel_heights = c(9, 3))

q <- ggdraw(p) +
  draw_label("2020 Superbowl Commercials", x = 0.5, y = 0.95, hjust = 0.5, vjust = 0, fontfamily = "Berlin Sans FB", color = "red", size = 14) +
  draw_label("N. Rennie | Data source: FiveThirtyEight", x = 0.15, y = 0.98, hjust = 0.5, vjust = 0, fontfamily = "Berlin Sans FB", color = "black", size = 6) 
q


ggsave(q, filename = "02032021.jpg")

