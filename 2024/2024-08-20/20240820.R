# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-08-20")
english_monarchs_marriages_df <- tuesdata$english_monarchs_marriages_df


# Load fonts --------------------------------------------------------------

font_add_google("Fraunces")
font_add_google("Ubuntu")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "#231942"
highlight_col <- "#005C69"
highlight_col2 <- "#B6174B"

body_font <- "Ubuntu"
title_font <- "Fraunces"


# Data wrangling ----------------------------------------------------------

monarch_data <- english_monarchs_marriages_df |>
  filter(
    str_detect(year_of_marriage, c("[–?]"), negate = TRUE),
    str_detect(consort_age, c("[–?]"), negate = TRUE),
    str_detect(king_age, c("[–?]"), negate = TRUE)
  ) |>
  mutate(across(contains("age"), as.numeric))

period_data <- tibble::tribble(
  ~Period, ~Start_Year, ~End_Year,
  "Anglo-Saxon Period", 802, 1066,
  "House of Normandy", 1066, 1154,
  "Angevins", 1154, 1216,
  "Plantagenets", 1216, 1399,
  "House of Lancaster", 1399, 1461,
  "House of York", 1461, 1485,
  "Tudors", 1485, 1603,
  "Stuart Period", 1603, 1714,
  "Hanoverians", 1714, 1901,
  "House of Saxe-Coburg and Gotha", 1901, 1917,
  "House of Windsor", 1917, 2020
) |>
  mutate(
    alpha = rep(c(0.1, 0.2), 6)[1:11]
  )



# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-08-20", "recording"),
  device = "png",
  width = 7,
  height = 10,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  twitter = NA,
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "English Monarchs and Marriages"
st <- glue("Data from ianvisits.co.uk lists the ages of <span style='color:{highlight_col}'>**monarchs**</span> and their 
<span style='color:{highlight_col2}'>**consorts**</span> at the time of marriage. 
Monarchs (who have historically mostly been male) are typically 
older than their consorts and *'it was not that uncommon for older monarchs to be 
married off to quite young women if needing a replacement queen after the 
previous model had been inconvenient enough to drop dead'*.
<br><br>*Only marriages where the monarch's age, the consort's age, and the 
year of marriage are known are included here. A further 27 marriages are included 
in the full data where the dates or ages are unknown or uncertain.")
cap <- paste0(
  "**Data**: Ian Visits & Wikipedia<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  # Period data
  geom_rect(
    data = period_data,
    mapping = aes(
      xmin = 0, xmax = 100,
      ymin = Start_Year, ymax = End_Year,
      alpha = alpha
    ),
    fill = text_col
  ) +
  geom_text(
    data = period_data,
    mapping = aes(
      x = mean(c(65, 100)),
      y = 0.5 * (Start_Year + End_Year),
      label = Period
    ),
    colour = text_col,
    family = title_font,
    size = 9
  ) +
  geom_text(
    data = period_data,
    mapping = aes(
      x = 101,
      y = Start_Year,
      label = Start_Year
    ),
    colour = text_col,
    family = body_font,
    hjust = 0,
    vjust = 1,
    size = 9
  ) +
  # Marriage data
  geom_segment(
    data = monarch_data,
    mapping = aes(
      x = king_age, xend = consort_age,
      y = year_of_marriage, yend = year_of_marriage
    ),
    colour = alpha(text_col, 0.5),
    size = 0.7
  ) +
  geom_point(
    data = monarch_data,
    mapping = aes(x = king_age, y = year_of_marriage),
    colour = highlight_col,
    size = 2,
    pch = 17
  ) +
  geom_point(
    data = monarch_data,
    mapping = aes(x = consort_age, y = year_of_marriage),
    colour = highlight_col2,
    size = 2
  ) +
  # Annotations
  geom_textbox(
    data = data.frame(
      x = 7, y = 1000,
      label = "Henry the Young King (age 5) marries Margaret of France (age 3) in 1160."
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    size = 9,
    hjust = 0.5,
    halign = 0.5,
    lineheight = 0.5,
    fill = alpha(bg_col, 0.3),
    width = unit(0.75, "inch")
  ) +
  annotate(
    "curve", x = 11, y = 1130, 
    xend = 7, yend = 1160,
    arrow = arrow(length = unit(0.2,"cm"), type = "closed"),
    colour = text_col,
    curvature = -0.5
  ) +
  geom_textbox(
    data = data.frame(
      x = 53, y = 1600,
      label = "The weddings of Henry the VIII."
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    size = 9,
    hjust = 0.5,
    halign = 0.5,
    lineheight = 0.5,
    fill = alpha(bg_col, 0.3),
    width = unit(1, "inch")
  ) +
  annotate(
    "curve", x = 43, y = 1600, 
    xend = 39, yend = 1580,
    arrow = arrow(length = unit(0.2,"cm"), type = "closed"),
    colour = text_col,
    curvature = -0.5
  ) +
  geom_textbox(
    data = data.frame(
      x = 50, y = 1420,
      label = "40 year age gap between Edward I (age 60) and Margaret of France (age 20)."
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    size = 9,
    hjust = 0.5,
    halign = 0.5,
    lineheight = 0.5,
    fill = alpha(bg_col, 0.3),
    width = unit(1, "inch")
  ) +
  annotate(
    "curve", x = 40, y = 1370, 
    xend = 36, yend = 1330,
    arrow = arrow(length = unit(0.2,"cm"), type = "closed"),
    colour = text_col,
    curvature = -0.5
  ) +
  # Styling
  scale_x_continuous(
    limits = c(0, 106),
    breaks = seq(0, 65, 5),
    minor_breaks = NULL
  ) +
  scale_y_reverse(limits = c(2020, 802)) +
  scale_alpha_identity() +
  coord_cartesian(expand = FALSE) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 26, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = alpha(text_col, 0.2),
      linewidth = 0.4
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = title_font,
      size = rel(2),
      face = "bold"
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-08-20", paste0("20240820", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
