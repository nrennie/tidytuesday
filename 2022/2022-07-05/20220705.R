library(tidyverse)
library(ggstream)
library(lubridate)
library(patchwork)
library(showtext)
library(rcartocolor)

# load data
rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

# add fonts
font_add_google(name = "Allan", family = "allan")
showtext_auto()

# prep data
plot_data <- rent %>%
  filter(!is.na(beds),
         !is.na(date)) %>%
  mutate(year_mon = round_date(ymd(date), unit = "months")) %>%
  select(year_mon, beds, price) %>%
  filter(beds %in% 0:6) %>%
  mutate(beds = factor(beds)) %>%
  group_by(year_mon, beds) %>%
  summarise(median_price = median(price))

# subtitles and caption
cap <- "N. Rennie | Data: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018."
st1 <- stringr::str_wrap("Between 2001 and 2018, the median rental cost of a 2 bedroom increased from $1,975 to $2,795",
                         22)
st2 <- stringr::str_wrap("Mid-2017 drop in median cost of 6 bedroom rentals",
                         24)

# label df
labels_df <- tibble(x = rep(round_date(ymd("20190101"), unit = "months"), 7),
                        y = c(15000, 11000, 8000, 4000, -2000, -10000, -14000),
                        label = paste0(unique(plot_data$beds), " Bedrooms"))
# plot
ggplot(data = plot_data,
       mapping = aes(x = year_mon, y = median_price)) +
  geom_stream(aes(fill = beds), bw = 0.5, extra_span = 0.001, sorting = "onset") +
  geom_text(data = labels_df,
            mapping = aes(x = x, y = y, label = label, colour = label),
            hjust = 0,
            size = 5,
            family = "allan") +
  scale_x_continuous(breaks = c(round_date(ymd(c("20040101", "20080101", "20120101", "20160101")),
                                           unit = "months")),
                     labels = c("2004", "2008", "2012", "2016"),
                     limits = c(round_date(ymd(c("20000101", "20220101")),
                                           unit = "months"))) +
  scale_y_continuous(limits = c(-28000, 28000)) +
  scale_fill_carto_d(palette = "Prism", direction = -1) +
  scale_colour_carto_d(palette = "Prism", direction = -1) +
  annotate("text",
           x = round_date(ymd("20080101"), unit = "months"),
           y = -20000,
           size = 5,
           label = "2008\nFinancial\nCrisis",
           family = "allan") +
  annotate("text",
           x = round_date(ymd("20030101"), unit = "months"),
           y = 18000,
           size = 5,
           label = st1,
           family = "allan") +
  annotate("text",
           x = round_date(ymd("20170601"), unit = "months"),
           y = 26000,
           size = 5,
           label = st2,
           family = "allan") +
  labs(x = "", y = "",
       title = "Median Rental Costs in San Francisco",
       subtitle = cap) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "allan", size = 28,
                                  margin = margin(b = 10)),
        plot.subtitle = element_text(hjust = 0.5, family = "allan", size = 14,
                                     margin = margin(b = 20)),
        axis.text.x = element_text(hjust = 0.5, family = "allan", size = 14,
                                     margin = margin(b = 10)),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"),
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"))
