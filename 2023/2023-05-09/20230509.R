library(tidyverse)
library(lubridate)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(ggmagnify)

# load fonts
font_add_google("Fredericka the Great", "fred")
showtext_auto()

# read in data
childcare_costs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv")
counties <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv")

# prep data
plot_data <- childcare_costs |>
  select(
    county_fips_code, study_year,
    mcsa, mhi_2018
  ) |>
  drop_na() |>
  left_join(counties, by = "county_fips_code") |>
  filter(state_name == "Texas") |>
  mutate(
    mhi_weekly = mhi_2018 / 52,
    mcsa_prop = mcsa / mhi_weekly
  )

# start recording
gg_record(
  dir = file.path("2023", "2023-05-09", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 8, # width of saved image
  height = 6.5, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "#F9FBF2"
highlight_col <- "#FFAD05"
dark_col <- "#616283"

# text
title <- "Childcare Costs in Texas"
st_text <- "The median weekly cost of full-time center-based care
for children of school age in Texas counties increased between 2009 and 2018.
Household income did not keep pace with the increasing cost of childcare, as
parents were spending an increasing proportion of their household income. In
some cases, the cost of childcare was almost 25% of the median weekly income,
and for those with lower household incomes, this figure is even higher."
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = dark_col,
  font_family = "fred"
)
st <- paste0(social, "<br><br>", st_text)
cap <- "**Source**: National Database of Childcare Prices<br>
*Median household income expressed in 2018 dollars."

# plot
set.seed(12345)
p <- ggplot(
  data = plot_data,
  mapping = aes(
    x = mcsa_prop,
    y = study_year,
    group = study_year)
) +
  geom_jitter(
    colour = highlight_col,
    size = 0.6
  ) +
  geom_boxplot(
    outlier.shape = NA,
    colour = highlight_col,
    fill = highlight_col,
    alpha = 0.3
  ) +
  scale_y_continuous(
    breaks = 2009:2018
  ) +
  scale_x_continuous(
    breaks = c(0.05, 0.1, 0.15, 0.2, 0.25),
    limits = c(0.04, 0.305)
  ) +
  labs(
    x = "Proportion of median weekly household income*",
    y = "",
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_minimal(
    base_size = 26,
    base_family = "Commissioner"
  ) +
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.4,
      colour = alpha(dark_col, 0.2),
    ),
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    plot.title = element_text(
      family = "fred",
      margin = margin(b = 20),
      colour = dark_col,
      size = 50
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.55,
      colour = dark_col,
      family = "Commissioner",
      hjust = 0,
      margin = margin(b = 20),
      halign = 0
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.55,
      colour = dark_col,
      margin = margin(t = 10)
    ),
    axis.text = element_text(colour = dark_col),
    axis.title = element_text(colour = dark_col),
    legend.position = "none",
    plot.margin = margin(15, 15, 10, 10)
  )

# magnify region
from <- c(0.24, 2016, 0.25, 2018)
to <- c(0.255, 2009, 0.3, 2018.5)
p_inset <- p + 
  geom_text(data = filter(plot_data, county_name == "Brooks County"),
            mapping = aes(
              label = paste0(round(100*mcsa_prop, 1), "%",
                             "\nBrooks County\n/ Kenedy County\n(", study_year, ")")),
            colour = dark_col,
            family = "Commissioner",
            size = 7,
            vjust = -1.5,
            lineheight = 0.5)
p +
  geom_magnify(
    from = from,
    to = to,
    colour = dark_col,
    shadow = TRUE,
    plot = p_inset
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-05-09", "20230509.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
