library(tidyverse)
library(patchwork)
library(countrycode)
library(extrafont)
library(cowplot)
tuesdata <- tidytuesdayR::tt_load('2021-07-06')
holidays <- tuesdata$holidays

#segment plot showing time since independence
holidays$continent <- countrycode(sourcevar = holidays$country, origin = "country.name", destination = "continent")
holidays$continent[131] <- "Oceania"
hol_data <- filter(holidays, !is.na(date_parsed))

#plot 1 
p1_data <- filter(hol_data, continent == "Africa")
p1_data <- p1_data %>% group_by(country) %>% summarise(date_parsed = min(date_parsed))
p1_data <- p1_data[order(p1_data$date_parsed),]
p1_data$country <- factor(p1_data$country, levels=rev(p1_data$country))
p1_data$col <- as.character(c(1, rep(2, nrow(p1_data)-1)))
p1 <- ggplot() +
  geom_segment(data=p1_data, mapping=aes(x=date_parsed, xend=as.Date("2021-07-06"), y=country, yend=country, colour=col), size=2) +
  geom_text(data=p1_data, mapping=aes(x=date_parsed, y=country, label=country), vjust=0.3, hjust=1, colour = "#AD235E", size=3.5, family="Eras Medium ITC") + 
  labs(x="", y="", subtitle="Africa") + 
  scale_x_date("", limits=c(as.Date("1100-01-01"), as.Date("2021-07-06"))) +
  scale_colour_manual("", values=c("1"="#AD235E", "2"="#7D9C9F")) +
  theme(plot.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        panel.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        legend.background = element_rect(fill = "#ECECEC"),
        legend.key = element_rect(fill = "#ECECEC", colour="#ECECEC"), 
        legend.text =  element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        legend.position="none",
        plot.subtitle = element_text(colour = "#AD235E", size=16, hjust = 0.5, family="Eras Bold ITC"),
        axis.text.x = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.text.y = element_blank(),
        axis.title = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1

#plot 2
p2_data <- filter(hol_data, continent == "Americas")
p2_data <- p2_data %>% group_by(country) %>% summarise(date_parsed = min(date_parsed))
p2_data <- p2_data[order(p2_data$date_parsed),]
p2_data$col <- as.character(c(1, rep(2, nrow(p2_data)-1)))
p2_data <- p2_data %>% add_row(country = as.character(1:(nrow(p1_data)-nrow(p2_data))), date_parsed = rep(as.Date("2021-07-06"), nrow(p1_data)-nrow(p2_data)), col=rep("2", nrow(p1_data)-nrow(p2_data)))
p2_text <- filter(p2_data, is.na(as.numeric(p2_data$country)))
p2_data$country <- factor(p2_data$country, levels=rev(p2_data$country))
p2 <- ggplot() +
  geom_segment(data=p2_data, mapping=aes(x=date_parsed, xend=as.Date("2021-07-06"), y=country, yend=country, colour=col), size=2) +
  geom_text(data=p2_text, mapping=aes(x=date_parsed, y=country, label=country), vjust=0.3, hjust=1, colour = "#AD235E", size=3.5, family="Eras Medium ITC") + 
  labs(x="", y="", subtitle="Americas") + 
  scale_x_date("", limits=c(as.Date("1100-01-01"), as.Date("2021-07-06"))) +
  scale_colour_manual("", values=c("1"="#AD235E", "2"="#7D9C9F")) +
  theme(plot.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        panel.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        legend.background = element_rect(fill = "#ECECEC"),
        legend.key = element_rect(fill = "#ECECEC", colour="#ECECEC"), 
        legend.text =  element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        legend.position="none",
        plot.subtitle = element_text(colour = "#AD235E", size=16, hjust = 0.5, family="Eras Bold ITC"),
        axis.text.x = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.text.y = element_blank(),
        axis.title = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2

#plot 3
p3_data <- filter(hol_data, continent == "Asia")
p3_data <- p3_data %>% group_by(country) %>% summarise(date_parsed = min(date_parsed))
p3_data <- p3_data[order(p3_data$date_parsed),]
p3_data$col <- as.character(c(1, rep(2, nrow(p3_data)-1)))
p3_data <- p3_data %>% add_row(country = as.character(1:(nrow(p1_data)-nrow(p3_data))), date_parsed = rep(as.Date("2021-07-06"), nrow(p1_data)-nrow(p3_data)), col=rep("2", nrow(p1_data)-nrow(p3_data)))
p3_text <- filter(p3_data, is.na(as.numeric(p3_data$country)))
p3_data$country <- factor(p3_data$country, levels=rev(p3_data$country))
p3 <- ggplot() +
  geom_segment(data=p3_data, mapping=aes(x=date_parsed, xend=as.Date("2021-07-06"), y=country, yend=country, colour=col), size=2) +
  geom_text(data=p3_text, mapping=aes(x=date_parsed, y=country, label=country), vjust=0.3, hjust=1, colour = "#AD235E", size=3.5, family="Eras Medium ITC") + 
  labs(x="", y="", subtitle="Asia") + 
  scale_x_date("", limits=c(as.Date("1100-01-01"), as.Date("2021-07-06"))) +
  scale_colour_manual("", values=c("1"="#AD235E", "2"="#7D9C9F")) +
  theme(plot.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        panel.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        legend.background = element_rect(fill = "#ECECEC"),
        legend.key = element_rect(fill = "#ECECEC", colour="#ECECEC"), 
        legend.text =  element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        legend.position="none",
        plot.subtitle = element_text(colour = "#AD235E", size=16, hjust = 0.5, family="Eras Bold ITC"),
        axis.text.x = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.text.y = element_blank(),
        axis.title = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p3

#plot 4
p4_data <- filter(hol_data, continent == "Europe")
p4_data <- p4_data %>% group_by(country) %>% summarise(date_parsed = min(date_parsed))
p4_data <- p4_data[order(p4_data$date_parsed),]
p4_data$col <- as.character(c(1, rep(2, nrow(p4_data)-1)))
p4_data <- p4_data %>% add_row(country = as.character(1:(nrow(p1_data)-nrow(p4_data))), date_parsed = rep(as.Date("2021-07-06"), nrow(p1_data)-nrow(p4_data)), col=rep("2", nrow(p1_data)-nrow(p4_data)))
p4_text <- filter(p4_data, is.na(as.numeric(p4_data$country)))
p4_data$country <- factor(p4_data$country, levels=rev(p4_data$country))
p4 <- ggplot() +
  geom_segment(data=p4_data, mapping=aes(x=date_parsed, xend=as.Date("2021-07-06"), y=country, yend=country, colour=col), size=2) +
  geom_text(data=p4_text, mapping=aes(x=date_parsed, y=country, label=country), vjust=0.3, hjust=1, colour = "#AD235E", size=3.5, family="Eras Medium ITC") + 
  labs(x="", y="", subtitle="Europe") + 
  scale_x_date("", limits=c(as.Date("1100-01-01"), as.Date("2021-07-06"))) +
  scale_colour_manual("", values=c("1"="#AD235E", "2"="#7D9C9F")) +
  theme(plot.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        panel.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        legend.background = element_rect(fill = "#ECECEC"),
        legend.key = element_rect(fill = "#ECECEC", colour="#ECECEC"), 
        legend.text =  element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        legend.position="none",
        plot.subtitle = element_text(colour = "#AD235E", size=16, hjust = 0.5, family="Eras Bold ITC"),
        axis.text.x = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.text.y = element_blank(),
        axis.title = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p4

#plot 5
p5_data <- filter(hol_data, continent == "Oceania")
p5_data <- p5_data %>% group_by(country) %>% summarise(date_parsed = min(date_parsed))
p5_data <- p5_data[order(p5_data$date_parsed),]
p5_data$col <- as.character(c(1, rep(2, nrow(p5_data)-1)))
p5_data <- p5_data %>% add_row(country = as.character(1:(nrow(p1_data)-nrow(p5_data))), date_parsed = rep(as.Date("2021-07-06"), nrow(p1_data)-nrow(p5_data)), col=rep("2", nrow(p1_data)-nrow(p5_data)))
p5_text <- filter(p5_data, is.na(as.numeric(p5_data$country)))
p5_data$country <- factor(p5_data$country, levels=rev(p5_data$country))
p5 <- ggplot() +
  geom_segment(data=p5_data, mapping=aes(x=date_parsed, xend=as.Date("2021-07-06"), y=country, yend=country, colour=col), size=2) +
  geom_text(data=p5_text, mapping=aes(x=date_parsed, y=country, label=country), vjust=0.3, hjust=1, colour = "#AD235E", size=3.5, family="Eras Medium ITC") + 
  labs(x="", y="", subtitle="Oceania") + 
  scale_x_date("", limits=c(as.Date("1100-01-01"), as.Date("2021-07-06"))) +
  scale_colour_manual("", values=c("1"="#AD235E", "2"="#7D9C9F")) +
  theme(plot.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        panel.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        legend.background = element_rect(fill = "#ECECEC"),
        legend.key = element_rect(fill = "#ECECEC", colour="#ECECEC"), 
        legend.text =  element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        legend.position="none",
        plot.subtitle = element_text(colour = "#AD235E", size=16, hjust = 0.5, family="Eras Bold ITC"),
        axis.text.x = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.text.y = element_blank(),
        axis.title = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p5

#blank plot
p6 <- ggplot() +
  theme(plot.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        panel.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        legend.background = element_rect(fill = "#ECECEC"),
        legend.key = element_rect(fill = "#ECECEC", colour="#ECECEC"), 
        legend.text =  element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        legend.position="none",
        plot.subtitle = element_text(colour = "#AD235E", size=16, hjust = 0.5, family="Eras Bold ITC"),
        axis.text.x = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.text.y = element_blank(),
        axis.title = element_text(colour = "#AD235E", size=12, family="Eras Medium ITC"),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#join plots together
p <- p1+p2+p3+p6+p4+p5 + plot_layout(ncol=2, nrow=3) & 
  theme(panel.background = element_rect(fill = "#ECECEC", colour="#ECECEC"),
        plot.background = element_rect(fill = "#ECECEC", colour="#ECECEC"))
p

q <- ggdraw() + 
  draw_plot(p) +
  draw_label(label="Independence\nDays", x=0.75, y=0.63, hjust=0.5, fontfamily="Eras Bold ITC", size=40, colour = "#AD235E") +
  draw_label(label="Swiss National Day celebrates the\nAnniversary of the Federal Charter\non August 1, 1291. Although it is\nthe earliest commemorated event, it\nwasn't celebrated until in 1891. \n\n*where countries have multiple\nnational/independence days, the\nearlier date is shown.\n\nN. Rennie | Data: Wikipedia",
             x=0.75, y=0.49, hjust=0.5, fontfamily="Eras Medium ITC", size=20, colour = "#AD235E") 


q
ggsave(q, filename="06072021.jpg", width=10,height=20,unit="in")
