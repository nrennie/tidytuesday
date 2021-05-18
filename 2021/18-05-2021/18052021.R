library(tidyverse)
library(extrafont)
library(patchwork)

dev.new(width=5,height=6,unit="in", noRStudioGD = TRUE)


tuesdata <- tidytuesdayR::tt_load('2021-05-18')
survey <- tuesdata$survey

degrees <- c("High School", "College degree", "Master's degree", "PhD" , "Professional degree (MD, JD, etc.)")
plot_data <- filter(survey, highest_level_of_education_completed %in% degrees & currency == "USD")
plot_data$highest_level_of_education_completed <- factor(plot_data$highest_level_of_education_completed, levels=degrees)

med_salary <- plot_data %>% group_by(highest_level_of_education_completed) %>% summarise(avg = median(annual_salary))
med_labels <- sapply(format(round(med_salary$avg),big.mark=",", trim=TRUE), function(x) paste("$", x, sep=""))
plot_data$other_monetary_comp <- as.numeric(plot_data$other_monetary_comp)
med_comp <- plot_data %>% group_by(highest_level_of_education_completed) %>% summarise(avg = median(other_monetary_comp, na.rm=T))
comp_labels <- sapply(format(round(med_comp$avg),big.mark=",", trim=TRUE), function(x) paste("$", x, sep=""))

#violin plot of salary based on level of education
p1 <- ggplot() +
  geom_violin(data=plot_data, aes(x=highest_level_of_education_completed, y=annual_salary, fill=highest_level_of_education_completed), draw_quantiles = c(0.5)) + 
  scale_fill_manual("",values=c("#EC576B", "#4EC5C1", "#E5E338", "#ec8e57", "#a76eef")) +
  ylim(-100000,350000) + coord_flip() +
  geom_text(data=data.frame(degrees, y=rep(-30000,5), 
                            lab=c("High\nSchool", "College\ndegree", "Master's\ndegree", "PhD" , "Professional\ndegree")), 
            aes(x=degrees, y=y, label=lab), family="Poor Richard", size=3) +
  geom_text(data=data.frame(degrees, y=rep(-90000,5), lab=med_labels), aes(x=degrees, y=y, label=lab), family="Poor Richard", size=3) +
  ggtitle("Annual Salary")+
  theme(panel.background = element_rect(fill = "gray95"),
        plot.background = element_rect(fill = "gray95"),
        legend.background = element_rect(fill = "gray95"),
        plot.title = element_text(colour = "black", size=18, face="bold", hjust = 0, family="Poor Richard"),
        plot.subtitle = element_text(colour = "black", size=13, hjust = 0.5, family="Poor Richard"),
        plot.caption = element_text(colour = "black", size=10, hjust = 1, family="Poor Richard"),
        legend.position="none",
        plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "#a8ddb5", fill="#060405"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#a8ddb5", size=12, family="Poor Richard", hjust = 0.5),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )

p1


p2 <- ggplot() +
  geom_violin(data=plot_data, aes(x=highest_level_of_education_completed, y=other_monetary_comp, fill=highest_level_of_education_completed), draw_quantiles = c(0.5)) + 
  scale_fill_manual("",values=c("#EC576B", "#4EC5C1", "#E5E338", "#ec8e57", "#a76eef")) +
  coord_flip() +
  scale_y_continuous(breaks=c(0, 100000, 200000, 300000), labels=c("0", "100K", "200K", "300K"), limits=c(-100000,350000)) +
  ggtitle("Additional Monetary Compensation")+
  geom_text(data=data.frame(degrees, y=rep(-30000,5), 
                            lab=c("High\nSchool", "College\ndegree", "Master's\ndegree", "PhD" , "Professional\ndegree")), 
            aes(x=degrees, y=y, label=lab), family="Poor Richard", size=3) +
  geom_text(data=data.frame(degrees, y=rep(-90000,5), lab=comp_labels), aes(x=degrees, y=y, label=lab), family="Poor Richard", size=3) +
  theme(panel.background = element_rect(fill = "gray95"),
        plot.background = element_rect(fill = "gray95"),
        legend.background = element_rect(fill = "gray95"),
        plot.title = element_text(colour = "black", size=18, face="bold", hjust = 0, family="Poor Richard"),
        plot.subtitle = element_text(colour = "black", size=13, hjust = 0.5, family="Poor Richard"),
        plot.caption = element_text(colour = "black", size=10, hjust = 1, family="Poor Richard"),
        legend.position="none",
        plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "#a8ddb5", fill="#060405"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#a8ddb5", size=12, family="Poor Richard", hjust = 0.5),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_text(colour = "black", size=9, family="Poor Richard"),
        axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )

p2

p <- p1 + p2 + plot_layout(nrow = 2) +
  plot_annotation(
    caption = 'N. Rennie | Data: Ask A Manager Survey', 
    title = 'DOES EDUCATION PAY?',
    subtitle="\nPeople with a professional degree (e.g. MD, JD) have the highest median salary at just under\n$120,000. They also take home the largest additional monetary compensations - another\n$5,000.") &
  theme(panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.background = element_rect(fill = "gray95", colour="gray95"),
        plot.title = element_text(colour = "black", size=16, face="bold", hjust = 0, family="Poor Richard"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Poor Richard"),
        plot.caption = element_text(colour = "black", size=10, hjust = 1, family="Poor Richard"))
p

ggsave(p, filename = "18052021.jpg",  bg = "transparent", height=6, width=5, unit="in")



