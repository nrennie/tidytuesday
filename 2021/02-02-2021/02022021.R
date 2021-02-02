library(readxl)
library(glue)
library(ggplot2)

hbcu_all <- read_excel("tabn313.20.xls", sheet = 1)
hbcu_black <- read_excel("tabn313.20.xls", sheet = 2)

d <- data.frame(year=hbcu_all$Year, males=hbcu_all$Males, total=hbcu_all$`Total enrollment`, black_males=hbcu_black$Males, black_females=hbcu_black$Females)
p <- ggplot(data = d, aes(year)) + 
  geom_line(aes(y = total),size=1, colour = "#c51b8a") + 
  geom_ribbon(aes(ymin = rep(0,length(year)), ymax = males-black_males, fill = "Male (Other)")) +
  geom_ribbon(aes(ymin = males-black_males, ymax = males, fill = "Male (Black)")) +
  geom_ribbon(aes(ymin = males, ymax = total-black_females, fill = "Female (Other)")) +
  geom_ribbon(aes(ymin = total-black_females, ymax = total, fill = "Female (Black)")) +
  scale_fill_manual("", values=c("Male (Black)"= "#6F90F7", "Male (Other)"= "#9fb5fa", "Female (Black)"="#F76F90", "Female (Other)"="#fa9fb5")) +
  labs(title="40 Years of Education", subtitle = "Data from Data.World and the National Center for Education Statistics (NCES) \non enrollment in Historically Black Colleges and Universities (HBCUs).", x = "",y="") + 
  ylim(0,500000) + 
  annotate("text", x = 2008, y = 450000, label = "2008", colour = "#c51b8a", size=6, fontface=2) +
  annotate("text", x = 2008, y = 400000, label = "Financial\nCrisis", colour = "#c51b8a", size=5) +
  geom_segment(aes(x = 2008, y = 330000, xend = 2008, yend = 360000), colour = "#c51b8a") +
  annotate("text", x = 1981, y = 360000, label = "1981", colour = "#c51b8a", size=6, fontface=2) +
  annotate("text", x = 1981, y = 310000, label = "White House\nInitiative on HBCUs", colour = "#c51b8a", size=5) +
  geom_segment(aes(x = 1981, y = 240000, xend = 1981, yend = 270000), colour = "#c51b8a") +
  annotate("text", x = 1996, y = 400000, label = "1996", colour = "#c51b8a", size=6, fontface=2) +
  annotate("text", x = 1996, y = 350000, label = "California and Texas\nban affirmative action", colour = "#c51b8a", size=5) +
  geom_segment(aes(x = 1996, y = 280000, xend = 1996, yend = 310000), colour = "#c51b8a") +
  theme(panel.background = element_rect(fill = "#fde0dd"),
        plot.background = element_rect(fill = "#fde0dd"),
        legend.background = element_rect(fill = "#fde0dd"),
        plot.title = element_text(colour = "#c51b8a", size=22, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour = "#c51b8a", size=12, hjust = 0.5),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.key = element_rect(size = 1.2, colour = "#c51b8a"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#c51b8a", size=12),
        axis.title.y= element_text(colour="#c51b8a", size=14),
        axis.text.x=element_text(colour = "#c51b8a", size=12),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p
ggsave(p, filename = "02022021.jpg")

