library(tidyverse)
library(ggstream)
library(extrafont)
library(patchwork)
tuesdata <- tidytuesdayR::tt_load('2021-06-22')
parks <- tuesdata$parks

#process data
parks$city[which(parks$city == "Washington, DC")] <- "Washington, D.C."
cities <- c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix")
choose_cities <- filter(parks, city %in% cities)
choose_cities$city <- factor(choose_cities$city, levels=cities)
choose_cities$park_pct_city_data <- as.numeric(gsub('.{1}$', '', choose_cities$park_pct_city_data))

#parameters for stream plots 
bwd <- rep(c(0.001, 0.01, 0.5, 1),4)
esp <- c(rep(0.0001,4), rep(0.001,4), rep(0.1,4), rep(1,4))
params <- tibble(bwd, esp)

#stream plots
p1 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[1], extra_span=params$esp[1], type="ridge") +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  scale_x_continuous(breaks=c(2014, 2020)) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[1], scientific = FALSE) , ",\nextra_span = ", format(params$esp[1], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p2 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[2], extra_span=params$esp[2], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[2], scientific = FALSE) , ",\nextra_span = ", format(params$esp[2], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p3 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[3], extra_span=params$esp[3], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[3], scientific = FALSE) , ",\nextra_span = ", format(params$esp[3], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p4 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[4], extra_span=params$esp[4], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[4], scientific = FALSE) , ",\nextra_span = ", format(params$esp[4], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p5 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[5], extra_span=params$esp[5], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[5], scientific = FALSE) , ",\nextra_span = ", format(params$esp[5], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p6 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[6], extra_span=params$esp[6], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[6], scientific = FALSE) , ",\nextra_span = ", format(params$esp[6], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p7 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[7], extra_span=params$esp[7], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[7], scientific = FALSE) , ",\nextra_span = ", format(params$esp[7], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p8 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[8], extra_span=params$esp[8], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[8], scientific = FALSE) , ",\nextra_span = ", format(params$esp[8], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p9 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[9], extra_span=params$esp[9], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[9], scientific = FALSE) , ",\nextra_span = ", format(params$esp[9], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p10 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[10], extra_span=params$esp[10], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[10], scientific = FALSE) , ",\nextra_span = ", format(params$esp[10], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p11 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[11], extra_span=params$esp[11], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[11], scientific = FALSE) , ",\nextra_span = ", format(params$esp[11], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p12 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[12], extra_span=params$esp[12], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[12], scientific = FALSE) , ",\nextra_span = ", format(params$esp[12], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p13 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[13], extra_span=params$esp[13], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[13], scientific = FALSE) , ",\nextra_span = ", format(params$esp[13], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p14 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[14], extra_span=params$esp[14], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[14], scientific = FALSE) , ",\nextra_span = ", format(params$esp[14], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p15 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[15], extra_span=params$esp[15], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[15], scientific = FALSE) , ",\nextra_span = ", format(params$esp[15], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

p16 <- ggplot(choose_cities, aes(x=year, y=park_pct_city_data, fill=city, group=city)) +
  geom_stream(color = NA, bw = params$bwd[16], extra_span=params$esp[16], type="ridge") +
  scale_x_continuous(breaks=c(2014, 2020)) +
  scale_fill_manual("",values=c("#730046", "#bfbb11", "#ffc200", "#e88801", "#c93c00")) +
  labs(x="", y="% of city area", subtitle=paste("bw = ", format(params$bwd[16], scientific = FALSE) , ",\nextra_span = ", format(params$esp[16], scientific = FALSE))) +
  theme(plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        legend.background = element_rect(fill = "ivory2"),
        strip.background =element_rect(fill="ivory2"),
        strip.text = element_text(colour = '#730046', family="Candara", size=12),
        legend.key = element_rect(fill = "ivory2", colour="ivory2"), 
        legend.text =  element_text(colour = "#730046", size=12, family="Candara Light"),
        legend.title =  element_text(colour = "#730046", size=12, family="Candara", hjust=0.5),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.subtitle = element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        axis.ticks = element_blank(),
        axis.text= element_text(colour = "#730046", size=10, hjust = 0.5, family="Candara Light"),
        panel.grid.minor = element_blank())

#dev.new(width=8,height=6.5,unit="in", noRStudioGD = TRUE)
p <-  p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14 + p15 + p16 + plot_layout(ncol = 4, nrow=4) +
  plot_annotation(
  caption = 'N.Rennie | Data: The Trust for Public Land',
  title = 'City parks: stream plot parameters')&
  theme(panel.background = element_rect(fill = "ivory2", colour="ivory2"),
        plot.background = element_rect(fill = "ivory2", colour="ivory2"),
        plot.title = element_text(colour = "#730046", size=24, face="bold", hjust = 0, family="Candara Light"),
        plot.caption = element_text(colour = "#730046", size=12, hjust = 0, family="Candara Light"))
p
