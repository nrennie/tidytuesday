library(tidyverse)
library(extrafont)
#library(patchwork)

tuesdata <- tidytuesdayR::tt_load('2021-08-10')
investment <- tuesdata$investment
ipd <- tuesdata$ipd
chain <- tuesdata$chain_investment

#president data
new_presidential <- presidential %>%  add_row(name="Truman", start=as.Date("1947-01-01"), end=as.Date("1953-01-20"), party="Democratic", .before = 1) %>%
                                  add_row(name="Trump", start=as.Date("2017-01-20"), end=as.Date("2021-01-20"), party="Republican")

#prep plot data
plot_data <- chain %>% filter(meta_cat %in% c("Health","Digital","Education","Conservation and development","Power")) %>% group_by(year, meta_cat) %>% summarise(total=sum(gross_inv_chain))
plot_data$new_date = as.Date(paste(plot_data$year, 1, 1, sep = "-")) 
plot_data$meta_cat <- recode(plot_data$meta_cat, "Conservation and development" = "Conservation")

#make plot
p <- ggplot() +
  geom_rect(data=new_presidential, mapping=aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=party, alpha=party)) +
  scale_fill_manual("", values=c("Democratic"="#0000ff", "Republican"="#e50000")) +
  scale_alpha_manual("", values=c("Democratic"=0.2, "Republican"=0.2)) +
  geom_line(data=plot_data, mapping=aes(x=new_date, y=total)) +
  facet_grid(meta_cat~., scales='free_y') +
  scale_x_date(limits=c(as.Date("1947-01-01"), as.Date("2022-01-01")), breaks=as.Date(c("1960-01-01", "1980-01-01", "2000-01-01", "2020-01-01")), labels=c("1960", "1980", "2000", "2020")) +
  coord_cartesian(expand=F) +
  labs(x="", y="Gross investment in millions of USD\n(adjusted for inflation)\n", 
       caption= "N. Rennie | Data: Bureau of Economic Analysis", 
       title="INFRASTRUCTURE\nINVESTMENT",
       subtitle="Whilst investment in digital, education, health, and power have\ncontinued to increase, investment in conservation has dropped\nsince the mid 1960s.\n") +
  theme(plot.background = element_rect(fill = "#FAF9F6", colour="#FAF9F6"),
        panel.background = element_rect(fill = "#FAF9F6", colour="#FAF9F6"),
        legend.background = element_rect(fill = "#FAF9F6", colour="#FAF9F6"),
        legend.key = element_rect(fill = NA),
        strip.background =element_rect(fill=alpha("#50487a",0.2)),
        strip.text = element_text(colour = '#50487a', family="Gill Sans MT", size=12),
        legend.position="bottom",
        panel.spacing = unit(1.5, "lines"),
        legend.margin=margin(t = 0, unit='cm'),
        axis.text = element_text(colour = "#50487a", size=12, hjust = 0.5, family="Bodoni MT"),
        axis.title = element_text(colour = "#50487a", size=14, hjust = 0.5, family="Bodoni MT"),
        legend.text = element_text(colour = "#50487a", size=12, hjust = 0.5, family="Bodoni MT"),
        plot.title = element_text(colour = "#50487a", size=28, hjust = 0, family="Bodoni MT Black"),
        plot.subtitle = element_text(colour = "#50487a", size=16, hjust = 0, family="Bodoni MT"),
        plot.caption = element_text(colour = "#50487a", size=12, hjust = 0.5, family="Bodoni MT"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
p



