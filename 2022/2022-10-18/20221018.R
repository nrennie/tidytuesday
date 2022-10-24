library(tidyverse)
library(camcorder)
library(showtext)
library(cowplot)

# load fonts
font_add_google("Ruluko", "ruluko")
showtext_auto()

# load data
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv')
all_dialogue <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')

# start recording
gg_record(
  dir = file.path("2022", "2022-10-18", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# prep data
get_ts <- function(s, e) {
  df_out <- all_dialogue %>% 
    filter(season == s, 
           episode == e) %>% 
    select(dialogue, start_time, end_time) %>% 
    mutate(start_time = as.integer(start_time),
           end_time = as.integer(end_time))
  len <- max(df_out$end_time)
  t <- seq_len(len)
  speaking <- rep(0, length = len)
  df_out <- df_out %>% 
    filter(!is.na(dialogue))
  for (i in 1:nrow(df_out)) {
    speaking[df_out$start_time[i]:df_out$end_time[i]] = 1
  }
  out <- tibble(t = t, speaking = speaking, season = paste0(s), id = paste0(s,"_",e))
  out <- out %>% 
    mutate(mins = factor(round(t/60))) %>% 
    group_by(mins) %>% 
    mutate(diag_sec = sum(speaking),
           speed = diag_sec/as.numeric(mins)) %>% 
    ungroup() %>% 
    mutate(mins = as.numeric(mins)) %>% 
    select(id, season, mins, speed) %>% 
    distinct() %>% 
    mutate(mins = mins/max(mins))
  return(out)
}

params <- all_dialogue %>% 
  select(season, episode) %>% 
  distinct()
output <- purrr::map(.x = 1:nrow(params), .f = ~get_ts(as.vector(params[.x, 1]),
                                                       as.vector(params[.x, 2])))
plot_data <- bind_rows(output)

# subtitle
st <- usefunc::str_wrap_break("Although episode lengths vary considerably by season, all episodes follow a similar pattern of how many seconds of dialogue occur per minute. There is a spike in density of dialogue at the start of the episode, before it gradually tails off.", 40)

# plot
p <- ggplot() +
  geom_line(data = plot_data, 
            mapping = aes(x = mins, 
                          y = speed, 
                          group = id), 
            size = 0.3,
            colour = alpha("#BEBEBE", 0.5)) +
  geom_smooth(data = plot_data, 
              mapping = aes(x = mins, 
                            y = speed), 
              colour = "#e21800", 
              size = 0.6,
              se = FALSE) +
  scale_x_continuous(limits = c(0, 1), 
                     breaks = c(0, 1), 
                     labels = c("Start of\nepisode", "End of\nepisode")) +
  labs(y = "Seconds of dialogue per minute", 
       tag = st) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(margin = margin(r = 10),
                                    colour = "#e21800", 
                                    family = "ruluko", 
                                    size = 30), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 8), units = "cm"), 
        plot.background = element_rect(fill = "#0c0f20", colour = "#0c0f20"),
        panel.background = element_rect(fill = "#0c0f20", colour = "#0c0f20"),
        legend.position = "none", 
        plot.tag = element_text(colour = "#e21800",
                                family = "ruluko", 
                                size = 30,
                                lineheight = 0.4),
        plot.tag.position = c(-0.6, 0.3),
        axis.text = element_text(colour = "#e21800", 
                                 family = "ruluko", 
                                 size = 28, 
                                 lineheight = 0.4),
        panel.grid = element_line(colour = alpha("#e21800", 0.4)))

# add logo
logo_file <- paste0(here::here(), "/2022/2022-10-18/logo.jpg")
ggdraw() + 
  draw_plot(p) +
  draw_image(
    logo_file, x = 0.43, y = 0.8, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.35
  )

# save gif
gg_playback(
  name = file.path("2022", "2022-10-18","20221018.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)














