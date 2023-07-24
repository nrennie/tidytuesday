library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(emojifont)

# load fonts
font_add_google("Ubuntu")
font_add_google("Archivo Narrow")
font_add_google("Archivo")
showtext_auto()

# load data
scurvy <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv")

# colours
bg_col <- "#202A44"
highlight_col <- "#C41E3A"
light_col <- "#ffffff"


# Version 1 ---------------------------------------------------------------

# prep data
plot_data <- scurvy |>
  group_by(fit_for_duty_d6) |>
  summarise(n = n())

# waffle data
waffle_data <- expand.grid(x = 1:3, y = 1:4) |>
  as_tibble() |>
  mutate(colour = c(
    rep(highlight_col, filter(plot_data, fit_for_duty_d6 == "0_no")$n),
    rep(light_col, filter(plot_data, fit_for_duty_d6 == "1_yes")$n)
  ))

# start recording
gg_record(
  dir = file.path("2023", "2023-07-25", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# text
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = light_col,
  font_colour = light_col,
  font_family = "Ubuntu"
)
cap <- paste0(
  "**Data**: A Treatise on the Scurvy in Three Parts. James Lind. 1757.<br>
  **Graphic**: ",
  social
  )

# text dataframe
text_df <- data.frame(
  x = rep(6, 5),
  y = rev(seq(1, 4, length.out = 5)),
  size = c(56, 24, 25, 36, 30),
  family = c("normal", "narrow", "narrow", "narrow", "narrow"),
  label = c("1  in  12", "sailors diagnosed", "with scurvy were", "fit for duty", "after six days."),
  colour = c(light_col, highlight_col, highlight_col, light_col, highlight_col)
)

# waffle chart
ggplot() +
  # anchor icons
  geom_text(
    data = waffle_data,
    mapping = aes(
      x = x, y = y, colour = colour,
      label = fontawesome("fa-anchor")
    ),
    size = 34,
    family = "fontawesome-webfont"
  ) +
  # add text
  geom_text(data = filter(text_df, family == "narrow"),
            mapping = aes(
              x = x,
              y = y,
              label = str_to_upper(label),
              colour = colour,
              size = size
            ),
            fontface = "bold",
            family = "Archivo Narrow") +
  geom_text(data = filter(text_df, family == "normal"),
            mapping = aes(
              x = x,
              y = y,
              label = str_to_upper(label),
              colour = colour,
              size = size
            ),
            fontface = "bold",
            family = "Archivo") +
  # styling
  scale_x_reverse(limits = c(7.5, 1)) +
  scale_y_continuous(limits = c(0.6, 4.4)) +
  scale_size_identity() +
  scale_colour_identity() +
  labs(caption = cap) +
  theme_void(base_size = 44, base_family = "Ubuntu") +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      hjust =  0,
      colour = light_col,
      size = 22,
      lineheight = 0.5,
      margin = margin(t = 50, b = -40, l = -25)
    ),
    plot.margin = margin(50, 40, 50, 40)
    )

# save gif
gg_playback(
  name = file.path("2023", "2023-07-25", "20230725.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)


# Version 2 ---------------------------------------------------------------

# text
social <- nrBrand::social_caption(
  bg_colour = light_col,
  icon_colour = bg_col,
  font_colour = bg_col,
  font_family = "Ubuntu"
)
cap <- paste0(
  "**Data**: A Treatise on the Scurvy in Three Parts. James Lind. 1757.<br>
  **Graphic**: ",
  social
)
st <- "In 1757, it was not known that scurvy is a manifestation of vitamin C 
deficiency. A variety of remedies had been anecdotally reported, but Lind 
was the first to test different regimens of acidic substances (including 
citrus fruits) against each other in a randomised controlled trial. This chart 
shows how many of the 12 sailors diagnosed with scurvy had moderate or severe 
symptoms by day six of their allocated treatment."

# number of 12 sailors who had moderate or severe symptoms by day 6 of scurvy.
plot_data <- scurvy |> 
  select(-study_id) |> 
  mutate(treatment = str_replace_all(treatment, "_", " "), 
         treatment = str_to_title(treatment),
         dosing_regimen_for_scurvy = str_to_title(str_replace_all(dosing_regimen_for_scurvy, "_", " ")), 
         treatment = glue::glue(
           "**{treatment}**<br>*{dosing_regimen_for_scurvy}*"
         ),
         # manual line breaks for y axis text
         treatment = str_replace_all(treatment, "Garlic, ", "Garlic,<br>"),
         treatment = str_replace_all(treatment, "Peru, ", "Peru,<br>")
  ) |> 
  select(-c(fit_for_duty_d6, dosing_regimen_for_scurvy)) |> 
  mutate(across(-treatment,
                ~ case_when(.x %in% c("2_moderate", "3_severe") ~ "Yes",
                            TRUE ~ "No"))) |> 
  pivot_longer(-treatment, names_to = "symptoms") |> 
  group_by(treatment, symptoms, value) |> 
  mutate(n = n(),
         symptoms = str_to_title(str_replace_all(symptoms, "_", " ")),
         symptoms = str_replace_all(symptoms, " D6", "")) |> 
  ungroup() |> 
  complete(treatment, symptoms, value) |> 
  mutate(n = replace_na(n, 0)) |> 
  filter(value == "Yes") |> 
  select(-value) |> 
  mutate(treatment = factor(treatment))

# plot
ggplot(data = plot_data,
       mapping = aes(x = symptoms, y = treatment, size = factor(n),
                     fill = treatment,
                     colour = treatment)) +
  geom_point(data = filter(plot_data, n > 0),pch = 21, alpha = 0.3) +
  scale_x_discrete(position = "top",
                   labels = function(x) stringr::str_wrap(x, width = 15)) +
  scale_y_discrete(drop = FALSE) +
  labs(x = NULL,
       y = NULL,
       title = "The Treatment of Scurvy in 1757",
       subtitle = st,
       caption = cap) +
  theme_minimal(base_size = 28, base_family = "Ubuntu") +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  scale_colour_brewer(palette = "Dark2", guide = "none") +
  guides(size = guide_legend(nrow = 1, title = "", label.position = "bottom")) +
  theme(legend.position = c(-0.15, 1.1),
        axis.text.y = element_markdown(lineheight = 0.5),
        axis.text.x = element_text(lineheight = 0.4, vjust = 0),
        panel.grid.major = element_line(linewidth = 0.3),
        plot.background = element_rect(fill = light_col, colour = light_col),
        panel.background = element_rect(fill = light_col, colour = light_col),
        plot.title = element_textbox_simple(
          hjust =  0,
          colour = bg_col,
          face = "bold",
          lineheight = 0.5,
          margin = margin(b = 20)
        ),
        plot.subtitle = element_textbox_simple(
          hjust =  0,
          colour = bg_col,
          lineheight = 0.5,
          margin = margin(b = 20)
        ),
        plot.caption = element_textbox_simple(
          hjust =  0,
          colour = bg_col,
          lineheight = 0.5,
          margin = margin(t = 10)
        ),
        plot.caption.position = "plot",
        plot.title.position = "plot"
  )

