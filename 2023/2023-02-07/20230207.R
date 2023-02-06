library(tidyverse)
library(showtext)
library(camcorder)
library(lubridate)
library(ggsankey)
library(ggtext)
library(glue)

# load fonts
font_add_google("Fraunces", "fraunces")
font_add_google("Commissioner", "commissioner")
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

# load data
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

# vertical lines
line_data = tibble(x = seq(2010, 2022, 2),
                   xend = seq(2010, 2022, 2),
                   y = 0,
                   yend = -1800)

# data wrangling
stock_data <- big_tech_stock_prices |> 
  mutate(year = year(date)) |> 
  group_by(stock_symbol, year) |> 
  summarise(open = mean(open, na.rm = TRUE)) |> 
  ungroup() |> 
  filter(year <= 2022)

stock_orders <- stock_data |> 
  filter(year == 2022) |> 
  arrange(open) |> 
  pull(stock_symbol)

plot_data <- stock_data |> 
  mutate(stock_symbol = factor(stock_symbol, levels = stock_orders)) |> 
  arrange(stock_symbol)

# start recording
gg_record(
  dir = file.path("2023", "2023-02-07", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
col_choices = c(grey.colors(n = 13, start = 0.1, end = 0.9), "#fb0f01")
names(col_choices) = stock_orders

# subtitle
social = "<span style='font-family:\"Font Awesome 6 Brands\";color:#fb0f01;'>&#xf099;</span><span style='color:white;'>.</span><span style='font-family:commissioner;color:#2F4F4F;'>@nrennie35</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#fb0f01;'>&#xf4f6;</span><span style='color:white;'>.</span><span style='font-family:commissioner;color:#2F4F4F;'>fosstodon/@nrennie</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#fb0f01;'>&#xf09b;</span><span style='color:white;'>.</span><span style='font-family:commissioner;color:#2F4F4F;'>nrennie</span><span style='color:white;'>..</span>"
st = "Of the 14 tech companies considered, <span style='color: #fb0f01;}'>Adobe Inc.</span> has the<br>highest average daily stock price when the markets<br>opened, after overtaking Netflix in 2021.<br><br>Data: Yahoo Finance"
cap = glue("{social}<br><br>{st}")

# plot
ggplot(data = plot_data,
       mapping = aes(x = year,
                     value = open,
                     node = factor(stock_symbol, levels = stock_orders),
                     next_node = factor(stock_symbol, levels = stock_orders),
                     fill = factor(stock_symbol, levels = stock_orders))) +
  geom_segment(data = line_data,
               mapping = aes(x = x,
                             xend = xend,
                             y = y,
                             yend = yend,
                             group = x),
               colour = "#2F4F4F",
               linetype = "dashed",
               linewidth = 0.3,
               inherit.aes = FALSE) +
  geom_sankey_bump(space = 1,
                   color = "transparent",
                   smooth = 6,
                   alpha = 0.8) +
  scale_fill_manual(values = col_choices) +
  scale_x_continuous(breaks = seq(2010, 2022, 2),
                     expand = expansion(add = c(0.05, 0.5))) +
  scale_y_continuous(expand = expansion(add = c(0, 0)),
                     limits = c(-1800, 1600)) +
  labs(title = "The Rise of Adobe Inc.",
       tag = cap) +
  theme_minimal() +
  theme(text = element_text(family = "commissioner",
                            colour = "#546666"),
        plot.tag = element_markdown(size = 38, 
                                    lineheight = 0.4,
                                    hjust = 0,
                                    margin = unit(c(0, 0, 0.5, 0), "cm")),
        plot.tag.position = c(0.01, 0.72),
        plot.margin = margin(0, 10, 10, 20),
        plot.title = element_text(family = "fraunces",
                                  size = 80,
                                  colour = "#2F4F4F",
                                  margin = unit(c(1, 0, 0.5, 0), "cm")),
        plot.title.position = "plot",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 30, vjust = 2),
        axis.title = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"))

# save gif
gg_playback(
  name = file.path("2023", "2023-02-07","20230207.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
