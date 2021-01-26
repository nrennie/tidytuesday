tuesdata <- tidytuesdayR::tt_load('2021-01-26')
plastics <- tuesdata$plastics

#aggregate data by country for 2020 
plastics_2020 <- plastics[which(plastics$year == "2020"),]
plastics_country <- aggregate(x = plastics_2020[,5:11], by = list(plastics_2020$country), FUN = sum)
plastics_prop <- data.frame(country=plastics_country$Group.1, t(apply(plastics_country[,2:8], 1, function(x) x/sum(x, na.rm=T))))

#choose 9 countries to plot
choose_country <- c("United Kingdom of Great Britain & Northern Ireland", 
                    "Nigeria", 
                    "China", 
                    "Brazil", 
                    "Bangladesh",
                    "United States of America", 
                    "Australia",
                    "Switzerland",
                    "Benin")

#subset countries
plot_data <- data.frame(plastics_names=colnames(plastics_prop)[2:8], t(plastics_prop[which(plastics_prop$country %in% choose_country),])[2:8,])
colnames(plot_data) <- c("plastics_names", c("United Kingdom", 
                                             "Nigeria", 
                                             "China", 
                                             "Brazil", 
                                             "Bangladesh",
                                             "United States of America", 
                                             "Australia",
                                             "Switzerland",
                                             "Benin"))
plot_data$plastics_names <- factor(plot_data$plastics_names,levels = c("hdpe", "ldpe", "pet", "pp", "ps", "pvc", "o"))

plot_data_long <- gather(plot_data, key="measure", value="value", c("United Kingdom", 
                                                                    "Nigeria", 
                                                                    "China", 
                                                                    "Brazil", 
                                                                    "Bangladesh",
                                                                    "United States of America", 
                                                                    "Australia",
                                                                    "Switzerland",
                                                                    "Benin"))
#plot bar chart for each country
p <- ggplot(plot_data_long, aes(x=plastics_names, y=value, fill=plastics_names))+
  geom_bar(stat='identity')+
  scale_fill_discrete(name = "", labels = c("High density polyethylene", "Low density polyethylene", 
                                                "Polyester", "Polypropylene", "Polystyrene", "PVC", "Other")) + 
  facet_wrap(~measure) +
  labs(title = "Plastic Types", 
       subtitle = "Proportions of different types of plastics found during Break Free From Plastic \ncleanups in different countries.") + 
  xlab("") + ylab("Proportion of Total Plastic") + 
  theme(panel.background = element_rect(fill = "#152238"),
        plot.background = element_rect(fill = "#152238"),
        legend.background = element_rect(fill = "#152238"),
        strip.background =element_rect(fill="#152238"),
        strip.text = element_text(colour = "#ffffff", size=12),
        plot.title = element_text(colour = "#ffffff", size=18, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour = "#ffffff", size=12, hjust = 0.5),
        legend.title = element_blank(),
        legend.position="top",
        legend.key = element_rect(size = 1.2, colour = "#152238"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="#ffffff", size=10),
        panel.spacing = unit(3, "lines"),
        axis.title.y= element_text(colour="#ffffff", size=12),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p
ggsave(p, filename = "26012021.jpg")









