library(tidytuesdayR)
library(tidyverse)
library(countrycode)
#load data
tuesdata <- tidytuesdayR::tt_load('2021-01-05')
raw_df <- tuesdata$transit_cost

#maximum cost per km for each country
df <- raw_df %>% group_by(country) %>% top_n(1, cost_km_millions) 
#obtain country names
df$end_year <- as.numeric(df$end_year) 
df <- df %>% drop_na(c("country", "start_year", "end_year", "cost_km_millions"), any_of(vars))
df$country_name <- countrycode(df$country, "ecb", "country.name")
df$country_name[which(df$country == "UK")] <- "United Kingdom"
df <- distinct(df)
#plot
p <- ggplot(df, aes(colour=cost_km_millions)) +
  #plot project duration
  geom_segment(aes(x=as.numeric(start_year), xend=as.numeric(end_year), y=country_name, yend=country_name), size=4.5) +
  #colour by cost
  scale_colour_gradient2(low = "#ee8a82",
                        mid = "#c8586c",
    high = "#70284a",
    midpoint = 2000,
    space = "Lab",
    guide = "colourbar",
    aesthetics = "colour"
  ) + 
  #order countries 
  scale_y_discrete(limits = factor(df$country_name[order(df$start_year)], levels=df$country_name[order(df$start_year)])) +
  scale_x_continuous(breaks=seq(min(df$start_year), max(df$end_year), by = 3)) +
  #note current year
  geom_segment(aes(x = 2021, y = df$country_name[which.min(df$start_year)], xend = 2021, yend = df$country_name[which.max(df$start_year)]), colour = "#70284a", linetype="dashed", size=1.5) +
  #visualisation
  theme_classic() +
  theme(plot.background = element_rect(fill = "#fbe6c5"),
        panel.background = element_rect(fill = "#fbe6c5"),
        legend.background = element_rect(fill = "#fbe6c5"),
        plot.title = element_text(colour = "#70284a", size=18, face="bold"),
        plot.subtitle = element_text(colour = "#70284a", size=12),
        axis.text.x= element_text(colour="#70284a", size=12),
        axis.text.y= element_text(colour="#70284a", size=11)) +
  xlab("") + ylab("") +  labs(title = "Transit-infrastructure Costs", 
                              subtitle = "Construction timelines for the most expensive transit-infrastructure \nprojects in each country",
                              colour = "US Dollars \n(millions)/km") 
p
ggsave(p, filename = "05012021.jpg", bg = "transparent", height=10, width=8)




