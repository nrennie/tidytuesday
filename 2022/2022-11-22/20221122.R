library(tidyverse)
library(showtext)
library(camcorder)
library(viridis)
library(usefunc)
library(gghighlight)

# load fonts
font_add_google("Raleway", "raleway")
showtext_auto()

# load data
museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')

# do museums close more often in areas with higher deprivation?
museum_data <- museums %>% 
  select(Year_opened, Year_closed, Area_Deprivation_index) %>% 
  drop_na() %>% 
  separate(Year_opened, into = c("opened1", "opened2"), sep = ":") %>% 
  separate(Year_closed, into = c("closed1", "closed2"), sep = ":") %>% 
  mutate(across(c(opened1, opened2, closed1, closed2), as.numeric), 
         Area_Deprivation_index = factor(Area_Deprivation_index, levels = 1:10)) %>% 
  mutate(across(c(closed1, closed2), ~if_else(.x == 9999, NA_real_, .x))) %>% 
  mutate(closed = case_when(closed1 == closed2 ~ closed1,
                            closed1 != closed2 ~ round((closed2 + closed1)/2))) %>% 
  mutate(opened = case_when(opened1 == opened2 ~ opened1,
                            opened1 != opened2 ~ round((opened2 + opened1)/2))) %>% 
  select(Area_Deprivation_index, opened, closed) %>% 
  rename(deprivation = Area_Deprivation_index) %>% 
  arrange(deprivation)

# function to calculate number of open museums in a given year for a deprivation area
num_year <- function(year, dep) {
  df <- museum_data %>% 
    filter(deprivation == dep)
  num_opened <- df %>% 
    filter(opened <= year) %>% 
    nrow()
  num_closed <- df %>% 
    filter(closed <= year) %>% 
    nrow()
  return(num_opened - num_closed)
}

# map across deprivations and years since 1960
all_years <- 1960:2021
deps <- 1:10
output <- purrr::pmap(expand.grid(all_years, deps), ~num_year(year = .x, dep = .y))
results <- matrix(unlist(output), nrow = length(all_years), byrow = FALSE)
colnames(results) <- 1:10
plot_data <- results %>% 
  as_tibble() %>% 
  mutate(year = all_years) %>% 
  pivot_longer(-year, names_to = "deprivation", values_to = "museums") %>% 
  mutate(deprivation = factor(deprivation, levels = 1:10))

# start recording
gg_record(
  dir = file.path("2022", "2022-11-22", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# plot text
cap <- str_wrap_break("* The Index of Multiple Deprivation (IMD) measures the relative deprivation of geographic areas in the UK, aggregating different dimensions (income, employment, education, health, crime, housing, and living environment). The index ranges from 1 (most deprived) to 10 (least deprived).\n\n**In some instances it has been impossible to establish an exact opening or closing date for a museum. In these cases, museums’ opening and closing dates are taken to be the mid point of a specified range of possible dates.\n\nN. Rennie | Data: museweb.dcs.bbk.ac.uk", 175)
st <- str_wrap_break("The absolute number of museums in each level of deprivation* means little given the differing sizes of the areas, although in 1960 similar numbers of museums existed in each level. Since around 2000, the number of open museums has stagnated across all areas, regardless of deprivation index. However, the rate of growth prior to this stagnation is lower in more deprived areas.", 135)

# plot
ggplot() +
  geom_line(data = plot_data, 
            mapping = aes(x = year, 
                          y = museums,
                          fill = deprivation)) +
  facet_wrap(~deprivation, nrow = 2) +
  labs(title = "Are there fewer museums opening in more deprived areas?",
       x = "", 
       y = "Estimated number of open museums**",
       caption = cap, 
       subtitle = st) +
  scale_y_continuous(limits = c(0, 500)) +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_family = "raleway") +
  theme(legend.position = "none", 
        panel.spacing = unit(1, "lines"),
        plot.title.position = "plot", 
        plot.caption.position = "plot", 
        plot.margin = margin(10, 15, 10, 10),
        plot.caption = element_text(hjust = 0, lineheight = 0.4, size = 16),
        plot.subtitle = element_text(margin = margin(b = 10), lineheight = 0.4, size = 20),
        plot.title = element_text(lineheight = 0.4, face = "bold", size = 28),
        axis.title.y = element_text(margin = margin(r = 10), size = 20),
        strip.text = element_text(size = 20),
        axis.text = element_text(size = 16),
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"))

# save gif
gg_playback(
  name = file.path("2022", "2022-11-22","20221122.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)


# Re-worked version -------------------------------------------------------

# data
lookup <- plot_data %>% 
  filter(year == 1960) %>% 
  select(c(deprivation, museums))
new_plot_data <- plot_data %>% 
  left_join(lookup, by = "deprivation") %>% 
  rename(museums = museums.x,
         museums_1960 = museums.y) %>% 
  mutate(change = (100*(museums - museums_1960)/museums_1960)) %>% 
  select(year, deprivation, change)

# plot text
cap <- str_wrap_break("* The Index of Multiple Deprivation (IMD) measures the relative deprivation of geographic areas in the UK, aggregating different dimensions (income, employment, education, health, crime, housing, and living environment). The index ranges from 1 (most deprived) to 10 (least deprived).\n\n**In some instances it has been impossible to establish an exact opening or closing date for a museum. In these cases, museums’ opening and closing dates are taken to be the mid point of a specified range of possible dates.\n\nN. Rennie | Data: museweb.dcs.bbk.ac.uk", 175)
st <- str_wrap_break("The change in the estimated number of open museums since 1960 is significantly lower in areas with higher levels of deprivation.* Since around 2000, the number of open museums has stagnated across all areas, regardless of deprivation index. However, the rate of growth prior to this stagnation is lower in more deprived areas.", 135)

# plot
ggplot() +
  geom_line(data = new_plot_data, 
            mapping = aes(x = year, 
                          y = change,
                          colour = deprivation)) +
  gghighlight(use_direct_label = F) +
  facet_wrap(~deprivation, nrow = 2) +
  labs(title = "Are there fewer museums opening in more deprived areas?",
       x = "", 
       y = "% change in estimated number of\nopen museums since 1960**",
       caption = cap, 
       subtitle = st) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_colour_viridis(discrete = TRUE, direction = -1) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_family = "raleway") +
  theme(legend.position = "none", 
        panel.spacing = unit(1, "lines"),
        plot.title.position = "plot", 
        plot.caption.position = "plot", 
        plot.margin = margin(10, 15, 10, 10),
        plot.caption = element_text(hjust = 0, lineheight = 0.4, size = 16),
        plot.subtitle = element_text(margin = margin(b = 10), lineheight = 0.4, size = 20),
        plot.title = element_text(lineheight = 0.4, face = "bold", size = 28),
        axis.title.y = element_text(margin = margin(r = 10), size = 20, lineheight = 0.4),
        strip.text = element_text(size = 20),
        axis.text = element_text(size = 16),
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"))

# save gif
gg_playback(
  name = file.path("2022", "2022-11-22","20221122_2.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)



