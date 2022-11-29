library(tidyverse)
library(showtext)
library(camcorder)
library(ggsankey)
library(rcartocolor)
library(forcats)
library(ggtext)

# load fonts
font_add_google("Raleway", "raleway")
font_add_google("Passion One", "passion")
showtext_auto()

# load data
wcmatches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

# num matches played
team_data <- wcmatches %>%
  select(year, home_team, away_team) %>%
  pivot_longer(cols = c(home_team, away_team),
               names_to = "type",
               values_to = "team") %>%
  select(-type) %>%
  mutate(year = as.character(year)) %>%
  group_by(year) %>%
  count(team) %>%
  ungroup() %>%
  mutate(year = as.numeric(year))

winners <- worldcups %>%
  count(winner) %>%
  arrange(-n) %>%
  mutate(winner = factor(winner, levels = winner)) %>%
  pull(winner)

plot_data <- team_data %>%
  mutate(country = if_else(team %in% winners, team, "Other"),
         country = factor(country, levels = c(levels(winners), "Other")),
         team = factor(team),
         team = fct_relevel(team,
                            "Brazil", "Italy", "West Germany", "Argentina",
                            "France", "Uruguay", "England", "Germany", "Spain"))

# start recording
gg_record(
  dir = file.path("2022", "2022-11-29", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# vertical lines
line_data <- tibble(x = seq(1930, 2018, 8),
                    xend = seq(1930, 2018, 8),
                    y = 0,
                    yend = -80)

# set colours
col_choices = c(carto_pal(10, "Bold")[1:9], alpha("#212b31", 0.5))
names(col_choices) = c(levels(winners), "Other")

# subtitle
col_text <- glue::glue("<span style='color:{col_choices[1:9]}'>{names(col_choices)[1:9]}</span>,")
st <- "Between 1930 and 2018, nine countries have won the Men's FIFA World Cup: <span style='color:#7F3C8D'>Brazil</span>,
<span style='color:#11A579'>Italy</span>,<br><span style='color:#3969AC'>West Germany</span>,
<span style='color:#F2B701'>Argentina</span>, <span style='color:#E73F74'>France</span>,
<span style='color:#80BA5A'>Uruguay</span>, <span style='color:#E68310'>England</span>,
<span style='color:#008695'>Germany</span>, and <span style='color:#CF1C90'>Spain</span>,
with Brazil<br>winning five times. The plot shows the number of matches played by each team per<br>
tournament."


# plot
ggplot(data = plot_data,
       mapping = aes(x = year,
                     value = n,
                     node = team,
                     fill = country)) +
  geom_segment(data = line_data,
               mapping = aes(x = x,
                             xend = xend,
                             y = y,
                             yend = yend,
                             group = x),
               colour = "#212b31",
               linetype = "dashed",
               inherit.aes = FALSE) +
  geom_sankey_bump(space = 0, color = "transparent", smooth = 6) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(breaks = seq(1930, 2018, 8)) +
  scale_y_continuous(limits = c(-80, 80)) +
  scale_fill_manual(values = col_choices) +
  labs(title = "FIFA World Cup Matches 1930 - 2018",
       subtitle = st,
       caption = "N. Rennie | Data: FIFA World Cup",
       x = "",
       y = "") +
  theme(plot.title = element_text(size = 80, family = "passion", colour = "#b7c4cd", margin = margin(b = 10)),
        plot.subtitle = element_markdown(size = 30, family = "raleway", colour = "#b7c4cd", lineheight = 0.5),
        plot.caption = element_text(size = 30, family = "raleway", colour = "#b7c4cd", hjust = 0),
        plot.margin = margin(15, 15, 10, 10),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 20, family = "raleway", colour = "#b7c4cd"),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#36454f", colour = "#36454f"),
        panel.background = element_rect(fill = "#36454f", colour = "#36454f"))

# save gif
gg_playback(
  name = file.path("2022", "2022-11-29","20221129.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)

