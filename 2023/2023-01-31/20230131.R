library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(monochromeR)
library(glue)

# load fonts
font_add_google("Fraunces")
font_add_google("Commissioner")
showtext_auto()

# load data
cats_uk <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')

# data wrangling
plot_data <- cats_uk |> 
  select(tag_id, ground_speed) |> 
  group_by(tag_id) |> 
  summarise(avg_speed = mean(ground_speed, na.rm = TRUE)) |> 
  left_join(select(cats_uk_reference, c(tag_id, animal_id, hrs_indoors, age_years)),
            by = "tag_id") |> 
  select(-tag_id) |> 
  mutate(hrs_indoors = as.character(hrs_indoors <= 17.5),
         hrs_indoors = recode(hrs_indoors, 
                              "TRUE" = "Mostly outdoors or mixed",
                              "FALSE" = "Mostly indoors")) |> 
  drop_na()

# summary
p_summaries <- plot_data |> 
  group_by(hrs_indoors) |> 
  summarise(avg_speed = mean(avg_speed),
            avg_age = mean(age_years)) |> 
  mutate(avg_speed = round(avg_speed))

# outlier
p_outlier <- plot_data |> 
  filter(avg_speed > 4000)

p_outlier2 <- plot_data |> 
  filter(age_years > 15)

# start recording
gg_record(
  dir = file.path("2023", "2023-01-31", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 12, # width of saved image
  height = 9, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# Palettes ----------------------------------------------------------------
# Plot colours
cat_palette <- list("Mostly outdoors or mixed" = "#95B2B8",
                    "Mostly indoors" = "#914D76",
                    "dark_text" = "#2F4F4F",
                    "light_text" = "#546666")

# To get text colour variants
text_cols <- monochromeR::generate_palette("#2F4F4F",  "go_lighter",  n_colours = 4)


# Plot --------------------------------------------------------------------

# subtitle 
st <- glue("Only 7% of cats in the study spend more than 17.5 hours per day indoors.
           <span style='color:{cat_palette$`Mostly indoors`}'>Cats who spend more<br>than 17.5
           hours a day indoors</span> tend to be among the faster cats, having higher 
           average ground<br>speed compared to <span style='color:{cat_palette$`Mostly outdoors or mixed`}'>cats
           who spend less time indoors</span>! Age seems to have minimal impact on speed.")

# basic plot
basic_plot <- ggplot() +
  geom_point(data = plot_data,
             mapping = aes(x = age_years,
                           y = avg_speed,
                           colour = hrs_indoors),
             size = 4) +
  labs(title = "Indoor cats are fast - and they don't slow down!",
       subtitle = st, 20,
       x = "Age of cat (years)",
       y = "Average ground speed (m/s)")

# add custom theme elements
styled_plot <- basic_plot +
  scale_colour_manual(values = cat_palette) +
  theme_minimal() +
  theme(text = element_text(family = "Commissioner",
                            colour = cat_palette$light_text),
        plot.subtitle = element_markdown(size = 54, 
                                         lineheight = 0.4,
                                         margin = unit(c(0, 0, 0.5, 0), "cm")),
        plot.title = element_markdown(family = "Fraunces",
                                      size = 80,
                                      colour = cat_palette$dark_text,
                                      margin = unit(c(1, 0, 0.5, 0), "cm")),
        axis.text = element_text(colour = cat_palette$light_text, size = 48),
        axis.title = element_text(colour = cat_palette$dark_text, size = 46),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"))

# add annotations
annotated_plot <- styled_plot +
  # Summary
  geom_textbox(data = p_summaries,
               mapping = aes(x = avg_age, y = avg_speed,
                             label = glue("{hrs_indoors}<span style='color:{cat_palette$light_text}; 
                                font-size:38pt'><br>Average speed: {avg_speed} m/s<br>")),
               halign = 0.5, hjust = 0.5,
               size = 20, 
               lineheight = 0.5,
               family = "Commissioner",
               box.color = cat_palette$dark_text,
               alpha = 0.6,
               maxheight = unit(4, "lines"),
               minwidth = unit(16, "lines"),
               maxwidth = unit(20, "lines")) +
  # Outlier
  geom_textbox(data = p_outlier,
               mapping = aes(x = age_years, y = avg_speed,
                             label = glue("<span style='color:{cat_palette$`Mostly outdoors or mixed`}'>{animal_id}</span>
     <span style='color:{cat_palette$light_text}; font-size:38pt'><br>by name, magic by nature!
     One speedy cat has an average speed of {avg_speed} m/s!</span>")),
               halign = 0, hjust = 0.5,
               vjust = 1.5,
               size = 20, 
               lineheight = 0.4,
               family = "Commissioner",
               box.size = 2,
               box.color = NA,
               fill = cat_palette$light_text,
               alpha = 0.1,
               minwidth = unit(16, "lines"),
               maxwidth = unit(20, "lines")) +
  # Outlier 2
  geom_textbox(data = p_outlier2,
               mapping = aes(x = age_years, y = avg_speed,
                             label = glue("<span style='color:{cat_palette$`Mostly indoors`}'>{animal_id}</span>
     <span style='color:{cat_palette$light_text}; font-size:38pt'><br>was the oldest cat in the study, with an age of {age_years} years.</span>")),
     halign = 1, hjust = 0.95,
     vjust = -0.5,
     size = 20, 
     lineheight = 0.4,
     family = "Commissioner",
     box.color = NA,
     fill = cat_palette$light_text,
     alpha = 0.1,
     minwidth = unit(12, "lines"),
     maxwidth = unit(20, "lines"))

# arrows
annotated_plot +
  annotate(geom = "curve", 
           x = 2, 
           y = 4500, 
           xend = 3, 
           yend = 4900, 
           linewidth = 0.8,
           colour = cat_palette$`Mostly outdoors or mixed`,
           curvature = 0.5,
           arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate(geom = "curve", 
           x = 14.8, 
           y = 3200, 
           xend = 15.6, 
           yend = 2800, 
           linewidth = 0.8,
           colour = cat_palette$`Mostly indoors`,
           curvature = 0.5,
           arrow = arrow(length = unit(1.5, "mm"), type = "closed"))

# save gif
gg_playback(
  name = file.path("2023", "2023-01-31","20230131.gif"),
  first_image_duration = 4,
  width = 900, 
  height = 1200,
  units = "px",
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
