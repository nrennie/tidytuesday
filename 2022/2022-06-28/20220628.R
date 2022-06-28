library(tidyverse)
library(showtext)
library(PostcodesioR)
library(sf)
library(rnaturalearth) 
library(rnaturalearthdata)
library(ggdist)
library(patchwork)

scot <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf', continent="Europe")
scot <- scot %>% 
  filter(geounit == "Scotland")

# get data
paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')

# load fonts

# prep data 
paygap

plot_data <- paygap %>% 
  select(employer_name, address, diff_median_hourly_percent, company_number) %>% 
  mutate(is_scot = str_sub(company_number, 1, 2) == "SC") %>% 
  filter(is_scot == TRUE) %>%
  mutate(postcode = sub('.*\\,', '', address), 
         postcode = str_trim(postcode)) %>% 
  select(-c(company_number, address, is_scot))
plot_data

get_lat_long <- function(postcode) {
  d <- postcode_lookup(postcode)
  lon <- d$longitude
  lat <- d$latitude
  return(c(lat, lon))
}

lats <- numeric(length = nrow(plot_data))
lons <- numeric(length = nrow(plot_data))
for (i in seq_len(nrow(plot_data))) {
  coords <- get_lat_long(plot_data$postcode[i])
  lats[i] = coords[1]
  lons[i] = coords[2]
}
plot_data$lat = lats
plot_data$lon = lons

plot_data <- plot_data %>% 
  filter(!is.na(lat)) 
plot_data
#write.csv(plot_data, "paygap.csv", row.names = FALSE)

slice_min(plot_data, n = 1, diff_median_hourly_percent)
slice_max(plot_data, n = 1, diff_median_hourly_percent)

st = usefunc::str_wrap_break("Of the 2,211 registered Scottish companies included on the list, with a valid postcode, over 68% paid men more than women. On average they paid men 16% more. 16% of companies paid men more than 25% higher salaries. Global Energy (Group) Ltd were the worst offenders on the list, paying men 69% more than women.\n\n N. Rennie | Data: gender-pay-gap.service.gov.uk",
                             40)

# prep
p1 = ggplot() + 
  geom_sf(data = scot, colour = "#663399") +
  geom_point(data = plot_data, 
             aes(x = lon, y = lat, fill = diff_median_hourly_percent), 
             size = 3, 
             pch = 21,
             colour = "transparent",
             alpha = 0.5) +
  theme_void() +
  labs(title = "Scotland's Gender Pay Gap", 
       tag = st
       ) +
  guides(fill=guide_colorbar(title.position="top", 
                             title.hjust =0.5)) +
  scale_fill_gradient2(name = "Median % Pay Gap", high = "#663399", low = "#669933", 
                       mid = "#fafafa",
                       limits = c(-150, 150), breaks = c(-100, 0, 100)) +
  theme(legend.position = "bottom", 
        plot.margin = unit(c(0.5, 5, 0.5, -1), unit = "cm"), 
        plot.tag = element_text(size = 10, colour = "#663399", family="sans", face="italic"),
        plot.tag.position = c(1.3, 0.7) ,
        plot.title = element_text(colour = "#663399", size=28, hjust = 0, 
                                  vjust=0, family="serif", face="italic"), 
        legend.text = element_text(size = 10, colour = "#663399", family="sans", face="italic"),
        legend.title = element_text(size = 10, colour = "#663399", family="sans", face="italic"),
        )

p2 = ggplot(data = plot_data,
            mapping = aes(x = diff_median_hourly_percent)) +
  stat_gradientinterval(position = "dodge",
                        colour = NA,
                        fill = "#663399",
                        width = 1) +
  stat_halfeye(adjust = .3,
               width = .3,
               .width = 0,
               justification = -.3,
               point_colour = 'NA',
               slab_fill=NA,
               slab_colour='#663399',
               slab_size=0.4) +
  geom_boxplot(width = .15,
               outlier.shape = NA,
               fill='#fafafa', 
               colour = "#663399") +
  stat_dots(
    side = "left",
    dotsize = .8,
    justification = 1.15,
    binwidth = 1,
    colour='#663399'
  ) +
  scale_x_continuous(limits = c(-150, 150)) +
  guides(fill="none", alpha='none') +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black"),
        axis.text.y = element_blank(), 
        panel.grid = element_blank(),
        plot.title = element_text(family = "bungee", hjust = 0, size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0, size = 12, color = "black"),
        legend.position = "none",
        axis.ticks = element_blank()
  )

p = p1 + inset_element(p2, left = 0.9, right = 1.8, top = 0.5, bottom = 0.1)
p




