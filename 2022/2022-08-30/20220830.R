library(tidyverse)
library(camcorder)
library(usmap)
library(showtext)
library(geofacet)

# define notin
`%notin%` <- Negate(`%in%`)

# load fonts
font_add_google("Poppins", "poppins")
showtext_auto()

# start recording
gg_record(
  dir = "/home/nicola/Documents/R-nicola/playground/recording", # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# load data
pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')

# prep data
plot_data <- pell %>%
  select(STATE, AWARD, YEAR) %>%
  group_by(STATE, YEAR) %>%
  summarise(total = sum(AWARD, na.rm = TRUE)) %>%
  rename(abbr = STATE) %>%
  filter(abbr %notin% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))

plot_data <- left_join(plot_data,
                       select(statepop, c(fips, abbr, full)),
                       by = "abbr")

# subtitle
st <- usefunc::str_wrap_break("The {pell} package contains data about pell award distribution across universities and colleges within the United States from 1999 to 2017. This map shows the distribution of total award value per state for each year.", 120)

# plot map
ggplot(data = plot_data,
       mapping = aes(x = YEAR, y = total)) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,
           colour = "#182644", size = 0.1) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,
           colour = "#182644", size = 0.1) +
  annotate("segment", x=2017, xend=2017, y=-Inf, yend=Inf,
           colour = "#182644", size = 0.1) +
  annotate("segment", x=-Inf, xend=Inf, y=4000000000, yend=4000000000,
           colour = "#182644", size = 0.1) +
  geom_area(fill = "#6F1D1B") +
  geom_text(data = filter(plot_data, YEAR == 2000),
            mapping = aes(x = 2004,
                          y = 3000000000,
                          label = abbr),
            family = "poppins",
            size = 7,
            colour = "#182644") +
  facet_geo(~ abbr, grid = "us_state_grid2", label = "code") +
  scale_x_continuous(breaks = c(2000, 2010)) +
  scale_y_continuous(name = "Total award value per state ($)",
                     breaks = c(0, 3000000000),
                     label = scales::unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, signif(max(plot_data$total), 1))) +
  labs(title = "Pell Awards",
       subtitle = st,
       x = "",
       caption = "N. Rennie | Data: U.S. Department of Education | #TidyTuesday") +
  guides(fill = guide_colourbar(title.position="top",
                                title.hjust = 0.5)) +
  coord_cartesian(expand = F) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.15, 'cm'),
        plot.background = element_rect(fill = "#E8E5DA", colour = "#E8E5DA"),
        panel.background = element_rect(fill = "#E8E5DA", colour = "#E8E5DA"),
        legend.background = element_rect(fill = "#E8E5DA", colour = "#E8E5DA"),
        plot.title = element_text(family = "poppins",
                                  hjust = 0.5,
                                  size = 56,
                                  colour = "#182644",
                                  lineheight = 0.5,
                                  margin = margin(b = -5)),
        plot.caption = element_text(family = "poppins",
                                  hjust = 0.5,
                                  size = 16,
                                  colour = "#182644"),
        axis.title.y = element_text(family = "poppins",
                                    size = 16,
                                    margin = margin(r = -10),
                                    colour = "#182644"),
        axis.text = element_text(family = "poppins",
                                    hjust = 0.5,
                                    size = 12,
                                    colour = "#182644"),
        plot.subtitle = element_text(family = "poppins",
                                    hjust = 0.5,
                                    size = 18,
                                    margin = margin(t = 10,
                                                    b = 10),
                                    colour = "#182644",
                                    lineheight = 0.5),
        legend.title = element_text(family = "poppins",
                                    hjust = 0.5,
                                    size = 18,
                                    colour = "#182644"),
        legend.text = element_text(family = "poppins",
                                   hjust = 0.5,
                                   size = 16,
                                   colour = "#182644"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 1.5, 0.5, 0.5), unit = "cm"))
record_polaroid()

gg_playback(
  name = file.path("/home/nicola/Documents/R-nicola/playground/recording","pell.gif"),
  first_image_duration = 8,
  last_image_duration = 12,
  frame_duration = .25
)

