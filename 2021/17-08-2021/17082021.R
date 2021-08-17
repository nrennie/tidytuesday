library(tidyverse)
library(ggalluvial)
library(extrafont)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load('2021-08-17')
computer <- tuesdata$computer

#prep data
computer$char <- recode(computer$char, "Picard (Cont'D)" = "Picard")
computer$pri_type <- recode(computer$pri_type, "Password" = "Other", "Comment" = "Other", "Wake Word" = "Other")

select_char <- c("Picard", "Riker", "Data", "Beverly", "Geordi") 
plot_data <- filter(computer, char %in% select_char, pri_type != "Other") %>% group_by(char, pri_type) %>% summarise(Freq = n())

#sankey chart
p <- ggplot(as.data.frame(plot_data),
       aes(y = Freq, axis1 = char, axis2 = pri_type)) +
  geom_alluvium(aes(fill = char), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "black", alpha = 0.3, lwd=2) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(title="\n\n\n\n\n", subtitle="\n\n\nStar Trek: The Next Generation is a prime example\nof how people envisioned Voice User Interfaces (VUIs).\nThis Sankey chart shows how often different characters\nspoke, and what type of interactions they made.", 
       x="", y="", caption = "N. Rennie | Data: SpeechInteraction.org") +
  theme(plot.background = element_rect(fill = "#000000", colour="#000000"),
        panel.background = element_rect(fill = "#000000", colour="#000000"),
        legend.position="none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.subtitle = element_text(colour = "#226fc1", size=16, hjust = 0.5, family="Gadugi"),
        plot.caption = element_text(colour = "#226fc1", size=12, hjust = 0.5, family="Gadugi"),
        plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

dev.new(width=8,height=9,unit="in", noRStudioGD = TRUE)

#add images
logo <- "./images/logo.jpg"
beverly <- "./images/beverly.jpg"
data <- "./images/data.jpg"
geordi <- "./images/geordi.jpg"
picard <- "./images/picard.png"
riker <- "./images/riker.jpg"


q <- ggdraw() + 
  draw_plot(p) +
  draw_image(logo, x = 0.5, y = 0.96, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.5) +
  draw_image(beverly, x = 0.1, y = 0.58, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.07) +
  draw_image(data, x = 0.1, y = 0.48, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.07) +
  draw_image(geordi, x = 0.1, y = 0.38, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.07) +
  draw_image(picard, x = 0.1, y = 0.28, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.07) +
  draw_image(riker, x = 0.1, y = 0.18, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.07) +
  #text labels
  draw_label(x=0.9, y=0.42, hjust=0.5, "Command", color = "#226fc1", size = 10, fontfamily="Gadugi", angle=90) +
  draw_label(x=0.9, y=0.235, hjust=0.5, "Question", color = "#226fc1", size = 10, fontfamily="Gadugi", angle=90) +
  draw_label(x=0.9, y=0.15, hjust=0.5, "Statement", color = "#226fc1", size = 10, fontfamily="Gadugi", angle=90) 
q

