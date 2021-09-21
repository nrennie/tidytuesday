library(tidyverse)
library(extrafont)
library(patchwork)
library(cowplot)
library(ggalluvial)

tuesdata <- tidytuesdayR::tt_load('2021-09-21')
nominees <- tuesdata$nominees

#### five most nominated shows ####
shows <- names(sort(table(nominees$title),decreasing = T)[1:5])

#### prep data ####
plot_data <- nominees %>%
  filter(title %in% shows) %>%
  group_by(title, type) %>%
  summarise(n=n())
plot_data


#### heat map ####
x=1:12
y=1:75
data1 <- expand.grid(x=x, y=y)
data1$z <- c(rep("#f6d271", filter(plot_data, title=="The Amazing Race" & type == "Nominee")$n), 
            rep("#f1b718", filter(plot_data, title=="The Amazing Race" & type == "Winner")$n),
            rep("grey15", 900-((filter(plot_data, title=="The Amazing Race" & type == "Nominee")$n)+filter(plot_data, title=="The Amazing Race" & type == "Winner")$n)))
p1 <- ggplot(data1, aes(x, y, fill= I(z), width=.7, height=.7)) + 
  geom_tile() +
  labs(subtitle="\nThe\nAmazing\nRace") +
  theme(panel.background = element_rect(fill = "grey15", colour="grey15"),
        plot.background = element_rect(fill = "grey15", colour="grey15"),
        legend.background = element_rect(fill = "grey15"),
        legend.key = element_rect(fill = "grey15", colour="grey15"), 
        legend.text =  element_text(colour = "black", size=12, family="Ebrima"),
        plot.subtitle = element_text(colour = "#f1b718", size=20, face="bold", hjust = 0.5, family="Ebrima"),
        legend.position=c(0.7, 0.13),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1


data2 <- expand.grid(x=x, y=y)
data2$z <- c(rep("#f6d271", filter(plot_data, title=="Game Of Thrones" & type == "Nominee")$n), 
             rep("#f1b718", filter(plot_data, title=="Game Of Thrones" & type == "Winner")$n),
             rep("grey15", 900-((filter(plot_data, title=="Game Of Thrones" & type == "Nominee")$n)+filter(plot_data, title=="Game Of Thrones" & type == "Winner")$n)))
p2 <- ggplot(data2, aes(x, y, fill= I(z), width=.7, height=.7)) + 
  geom_tile() +
  labs(subtitle="\nGame\nOf\nThrones") +
  theme(panel.background = element_rect(fill = "grey15", colour="grey15"),
        plot.background = element_rect(fill = "grey15", colour="grey15"),
        legend.background = element_rect(fill = "grey15"),
        legend.key = element_rect(fill = "grey15", colour="grey15"), 
        legend.text =  element_text(colour = "black", size=12, family="Ebrima"),
        plot.subtitle = element_text(colour = "#f1b718", size=20, face="bold", hjust = 0.5, family="Ebrima"),
        legend.position=c(0.7, 0.13),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2


data3 <- expand.grid(x=x, y=y)
data3$z <- c(rep("#f6d271", filter(plot_data, title=="Dancing With The Stars" & type == "Nominee")$n), 
             rep("#f1b718", filter(plot_data, title=="Dancing With The Stars" & type == "Winner")$n),
             rep("grey15", 900-((filter(plot_data, title=="Dancing With The Stars" & type == "Nominee")$n)+filter(plot_data, title=="Dancing With The Stars" & type == "Winner")$n)))
p3 <- ggplot(data3, aes(x, y, fill= I(z), width=.7, height=.7)) + 
  geom_tile() +
  labs(subtitle="\nDancing\nWith\nThe Stars") +
  theme(panel.background = element_rect(fill = "grey15", colour="grey15"),
        plot.background = element_rect(fill = "grey15", colour="grey15"),
        legend.background = element_rect(fill = "grey15"),
        legend.key = element_rect(fill = "grey15", colour="grey15"), 
        legend.text =  element_text(colour = "black", size=12, family="Ebrima"),
        plot.subtitle = element_text(colour = "#f1b718", size=20, face="bold", hjust = 0.5, family="Ebrima"),
        legend.position=c(0.7, 0.13),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p3


data4 <- expand.grid(x=x, y=y)
data4$z <- c(rep("#f6d271", filter(plot_data, title=="The Voice" & type == "Nominee")$n), 
             rep("#f1b718", filter(plot_data, title=="The Voice" & type == "Winner")$n),
             rep("grey15", 900-((filter(plot_data, title=="The Voice" & type == "Nominee")$n)+filter(plot_data, title=="The Voice" & type == "Winner")$n)))
p4 <- ggplot(data4, aes(x, y, fill= I(z), width=.7, height=.7)) + 
  geom_tile() +
  labs(subtitle="\nThe\nVoice\n") +
  theme(panel.background = element_rect(fill = "grey15", colour="grey15"),
        plot.background = element_rect(fill = "grey15", colour="grey15"),
        legend.background = element_rect(fill = "grey15"),
        legend.key = element_rect(fill = "grey15", colour="grey15"), 
        legend.text =  element_text(colour = "black", size=12, family="Ebrima"),
        plot.subtitle = element_text(colour = "#f1b718", size=20, face="bold", hjust = 0.5, family="Ebrima"),
        legend.position=c(0.7, 0.13),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p4


data5 <- expand.grid(x=x, y=y)
data5$z <- c(rep("#f6d271", filter(plot_data, title=="Saturday Night Live" & type == "Nominee")$n), 
             rep("#f1b718", filter(plot_data, title=="Saturday Night Live" & type == "Winner")$n),
             rep("grey15", 900-((filter(plot_data, title=="Saturday Night Live" & type == "Nominee")$n)+filter(plot_data, title=="Saturday Night Live" & type == "Winner")$n)))
p5 <- ggplot(data5, aes(x, y, fill= I(z), width=.7, height=.7)) + 
  geom_tile() +
  labs(subtitle="\nSaturday\nNight\nLive") +
  theme(panel.background = element_rect(fill = "grey15", colour="grey15"),
        plot.background = element_rect(fill = "grey15", colour="grey15"),
        legend.background = element_rect(fill = "grey15"),
        legend.key = element_rect(fill = "grey15", colour="grey15"), 
        legend.text =  element_text(colour = "black", size=12, family="Ebrima"),
        plot.subtitle = element_text(colour = "#f1b718", size=20, face="bold", hjust = 0.5, family="Ebrima"),
        legend.position=c(0.7, 0.13),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p5


#join plots together
p <- p1 + p2 + p3 + p4 + p5 + plot_layout(ncol=5, nrow=1) +
  plot_annotation(caption="N.Rennie | Data: emmys.com\n", title="\n\n\n\n\n\n\n\nAlthough Game of Thrones has only won the fourth most Emmy awards,\nit has won more than half of the awards it has been nominated for.") &
  theme(panel.background = element_rect(fill = "grey15", colour="grey15"),
              plot.background = element_rect(fill = "grey15", colour="grey15"),
        plot.caption = element_text(colour = "#f1b718", size=14, hjust = 0.5, family="Ebrima"),
        plot.title = element_text(colour = "#f1b718", size=16, hjust = 0.5, family="Ebrima"))
p

#add logos
logo <- "./images/emmy.png"
q <- ggdraw() + 
  draw_plot(p) +
  draw_image(logo, x = 0.5, y = 0.98, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.3)
q
