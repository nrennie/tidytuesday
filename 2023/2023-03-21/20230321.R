library(tidyverse)
library(lubridate)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)

# load fonts
font_add_google("VT323", "vt")
font_add_google("Share Tech Mono", "share")
showtext_auto()

# load data
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

# start recording
gg_record(
  dir = file.path("2023", "2023-03-21", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# data wrangling
plot_data <- languages |> 
  filter(type == "pl") |> 
  filter(line_comment_token %in% c("//", "#", ";")) |> 
  select(title, appeared, line_comment_token, last_activity, language_rank) |> 
  drop_na() |> 
  mutate(label = paste("Comment token:", line_comment_token)) |> 
  group_by(label) |> 
  slice_head(n = 10) |> 
  mutate(n = factor(row_number(), levels = 1:10)) |> 
  ungroup() |> 
  select(label, n, title, appeared, last_activity, language_rank) |> 
  pivot_longer(cols = c(appeared, last_activity),
               names_to = "type",
               values_to = "year") |>
  mutate(title = paste0("<p>", title, "</p><p style='font-size:19pt;'>(Rank: ", language_rank, ")</p>"))

# text
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#66FF00;'>&#xf099;</span><span style='color:grey5;'>.</span><span style='font-family:share;color:#66FF00;'>@nrennie35</span><span style='color:grey5;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#66FF00;'>&#xf4f6;</span><span style='color:grey5;'>.</span><span style='font-family:share;color:#66FF00;'>fosstodon.org/@nrennie</span><span style='color:grey5;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#66FF00;'>&#xf09b;</span><span style='color:grey5;'>.</span><span style='font-family:share;color:#66FF00;'>nrennie</span><span style='color:grey5;'>..</span>"
subtitle <- "Of the 4,303 programming languages listed in the Programming Language DataBase, 205 use //, 101 use #, and 64 use ;
to define which lines are comments. 3,831 languages do not have a comment token listed."

# plot
ggplot(plot_data) +
  geom_line(aes(x = year, y = n, group = n),
            colour = "#66FF00",
            linewidth = 0.4) +
  geom_point(aes(x = year, y = n),
             pch = 22,
             fill = "grey5",
             colour = "#66FF00",
             size = 2) +
  geom_richtext(aes(x = 1930, y = n, label = title),
            hjust = 0,
            family = "share",
            size = 8,
            fill = "transparent",
            label.colour = "transparent",
            vjust = 0.5,
            colour = "#66FF00") +
  facet_wrap(~label, nrow = 1) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(breaks = c(1970, 1990, 2010, 2030),
                     limits = c(1930, 2030),
                     expand = c(0, 0)) +
  labs(x = "",
       y = "",
       title = "Programming Languages",
       subtitle = subtitle,
       caption = social) +
  theme_minimal(base_size = 24,
                base_family = "share") +
  theme(plot.background = element_rect(fill = "grey5", colour = "grey5"),
        panel.background = element_rect(fill = "grey10",
                                        colour = alpha("#66FF00", 0.5),
                                        linewidth = 0.4),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = alpha("#66FF00", 0.5),
                                          linewidth = 0.2),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(colour = "#66FF00", size = 26),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "#66FF00",
                                   vjust = 2),
        plot.title = element_text(colour = "#66FF00",
                                  family = "vt",
                                  face = "bold",
                                  size = 60),
        plot.subtitle = element_textbox_simple(
          lineheight = 0.4,
          colour = "#66FF00",
          hjust = 0,
          size = 28,
          margin = margin(b = 5)
        ),
        plot.caption = element_textbox_simple(
          lineheight = 0.4,
          colour = "#66FF00",
          hjust = 0,
          size = 28,
          margin = margin(t = 5)
        ),
        axis.ticks = element_blank(),
        plot.margin = margin(10, 15, 5, 0))

# save gif
gg_playback(
  name = file.path("2023", "2023-03-21", "20230321.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "grey5"
)





















