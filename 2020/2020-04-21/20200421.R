library(tidyverse)
library(lubridate)
library(packcircles)
library(showtext)
library(usefunc)
library(MetBrewer)

# get data
tuesdata <- tidytuesdayR::tt_load('2020-04-21')
gdpr_violations <- tuesdata$gdpr_violations

#add fonts
font_add_google(name = "Libre Franklin", family = "libre")
showtext_auto()

# prep data
p_data <- gdpr_violations %>%
  filter(date != "01/01/1970") %>%
  mutate(date = mdy(date))
date_range <- range(p_data$date)
plot_data <- p_data %>%
  select(id, name, price) %>%
  group_by(name) %>%
  summarise(n = sum(price)) %>%
  filter(n > 0)
plot_data

label <- character(length = nrow(plot_data))
for (i in 1:nrow(plot_data)){
  if (plot_data$name[i] %in% c("Germany", "Italy", "France", "Sweden", "Austria")){
    label[i] = paste0(plot_data$name[i], "\n", "€", format(plot_data$n[i], big.mark=",", trim=TRUE))
  } else {
    label[i] = plot_data$name[i]
  }
}
plot_data$label <- label

# pack circles
packing <- circleProgressiveLayout(plot_data$n, sizetype='area')
data <- cbind(plot_data, packing)
plot_data_i <- circleLayoutVertices(packing, npoints=50)
plot_data_i$n <- rep(plot_data$n, each=51)

# subtitle
st <- str_wrap_break("The General Data Protection Regulation (EU) 2016/679 (GDPR) is a regulation in EU law on data protection and privacy in the European Union (EU) and the European Economic Area (EEA). It also addresses the transfer of personal data outside the EU and EEA areas. During the period from 12th December 2018 to 25th March 2020, fines for violating GDPR requirements exceeded 153 million euros.\n\nThe area of each circle represented the value of the fines incurred by each country in the given time period. Fines for which the date was mis-recorded are not included here.\n\nN. Rennie | Data: Privacy Affairs | #30DayChartChallenge", 100)

# plot
p <- ggplot() +
  geom_polygon(data = plot_data_i,
               mapping = aes(x, y, group = id, fill=n),
               colour = "#f5f9f9",
               alpha = 1) +
  geom_text(data = data,
            mapping = aes(x, y, size = n, label = str_wrap(label, 6)),
            family="libre",
            colour = "white",
            lineheight = 0.5, fontface = "bold") +
  scale_size_continuous(range = c(2,10)) +
  scale_fill_gradientn(colors=met.brewer("Hokusai2")) +
  coord_fixed() +
  labs(title = "GDPR Violations",
       subtitle = st) +
  theme(panel.background = element_rect(fill = "#f5f9f9", colour="#f5f9f9"),
        plot.background = element_rect(fill = "#f5f9f9", colour="#f5f9f9"),
        plot.subtitle = element_text(colour = "#003051", size=20,
                                     hjust = 0, family="libre",
                                     lineheight = 0.5),
        plot.title = element_text(colour = "#003051", size=56,
                                  face="bold", hjust = 0, family="libre",
                                  margin = margin(b = 10)),
        legend.position="none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2.5, "lines"))
p

ggsave(p, filename = "2022/viz/day_11.jpg", height = 7, width = 5, unit = "in")
