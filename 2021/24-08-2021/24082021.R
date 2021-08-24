library(tidyverse)
library(cowplot)
library(extrafont)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load('2021-08-24')
lemurs <- tuesdata$lemurs

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

#Collared brown lemur
ecol_data <- filter(lemurs, taxon == "ECOL")

#oldest lemurs
ecol_data2 <- unique(ecol_data[,c("name", "sex", "dob", "dod", "age_max_live_or_dead_y")]) %>% arrange(desc(age_max_live_or_dead_y))
ecol_data2$name <- recode(ecol_data2$name, "YVETTE"="Yvette", "HENRI"="Henri", "GILLES"="Gilles", "GERARD"="Gerard", "DOMINIQUE"="Dominique", "CLAIRE"="Claire")
ecol_data2$dod2 <- pmin(ecol_data2$dod, as.Date("2021-08-24"), na.rm=T)
ecol_data2$mid_date <- ecol_data2$dob + floor((ecol_data2$dod2-ecol_data2$dob)/2)
ecol_data2$age_text <- paste(round(ecol_data2$age_max_live_or_dead_y,1), " years", sep="")

#the oldest lemurs
p1 <- ggplot() +
  geom_point(data=ecol_data2[1:10,], mapping=aes(x=dob, y=reorder(name, age_max_live_or_dead_y), colour=sex), size=4) +
  geom_point(data=ecol_data2[1:10,], mapping=aes(x=dod, y=reorder(name, age_max_live_or_dead_y), colour=sex), size=4) +
  geom_segment(data=ecol_data2[1:10,], mapping=aes(x=dob, xend=dod2, y=reorder(name, age_max_live_or_dead_y), yend=reorder(name, age_max_live_or_dead_y), colour=sex), size=1.5) +
  geom_text(data=ecol_data2[1:10,], mapping=aes(x=mid_date, y=reorder(name, age_max_live_or_dead_y), label=age_text, colour=sex), vjust=-1, family="Gill Sans MT Condensed", size=5) +
  scale_colour_manual("", values=c("F" = "#b13173", "M" = "#21ADA8"), labels=c("Female", "Male")) +
  labs(x="", y="") +
  theme(plot.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        panel.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        legend.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        legend.position="none",
        legend.key = element_rect(fill="#F5F5DC"),
        legend.text = element_text(colour = "#4e2e12", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        axis.text = element_text(colour = "#4e2e12", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        axis.title = element_text(colour = "#4e2e12", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major.y = element_line(colour=alpha("#4e2e12", 0.3)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
p1

#scatter plot of age vs weight
p2 <- ggplot() +
  geom_point(data=ecol_data, mapping=aes(x=age_at_wt_mo, y=weight_g, colour=sex), size=1) +
  scale_colour_manual("", values=c("F" = "#b13173", "M" = "#21ADA8"), labels=c("Female", "Male")) +
  labs(x="\nAge (months)", y="Weight (g)\n") +
  theme(plot.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        panel.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        legend.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        legend.position="bottom",
        legend.key = element_rect(fill="#F5F5DC"),
        legend.text = element_text(colour = "#4e2e12", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        axis.text = element_text(colour = "#4e2e12", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        axis.title = element_text(colour = "#4e2e12", size=12, hjust = 0.5, family="Gill Sans MT Condensed"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_line(colour=alpha("#4e2e12", 0.3)),
        panel.grid.minor = element_blank())
p2

#blank plot
p3 <- ggplot() +
  theme(plot.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        panel.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        legend.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        legend.position="bottom",
        legend.key = element_rect(fill="#F5F5DC"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_line(colour=alpha("#b13173", 0.3)),
        panel.grid.minor = element_blank())
p3
  
#join plots
p0 <- p2 + p3 + plot_layout(nrow=2, ncol=1) 
p <- p1 + p0 +
  plot_annotation(title = "\nCollared Brown Lemur", 
                  caption = "N. Rennie | Data:  Duke Lemur Centre | Image:  Duke Lemur Centre", 
                  subtitle="\nAt Duke Lemur Centre, collared brown lemurs live to an average of 23.6 years. The oldest\ncollared brown lemur at Duke Lemur Centre was Yvette who was born in 1959 and lived\nto the age of 32.6 years. \n\nCollared brown lemurs mature at about 3 years old. Once fully-grown, females tend to\nweigh more than males.") &
  theme(plot.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        panel.background = element_rect(fill = "#F5F5DC", colour="#F5F5DC"),
        plot.title = element_text(colour = "#4e2e12", size=28, hjust = 0, family="Gill Sans Ultra Bold"),
        plot.subtitle = element_text(colour = "#4e2e12", size=18, hjust = 0, family="Gill Sans MT Condensed"),
        plot.caption = element_text(colour = "#4e2e12", size=12, hjust = 0, family="Gill Sans MT Condensed"))
p

#add lemur image
lemur <- "./images/lemur.png"
q <- ggdraw() + 
  draw_plot(p) +
  draw_image(lemur, x = 0.78, y = 0.27, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.6)
q


