
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(rcartocolor)
library(VoronoiPlus) #devtools::install_github("AllanCameron/VoronoiPlus")


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-09-12")
all_countries <- tuesdata$all_countries
country_regions <- tuesdata$country_regions
global_human_day <- tuesdata$global_human_day
global_economic_activity <- tuesdata$global_economic_activity


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Data wrangling ----------------------------------------------------------

uk_data <- all_countries |> 
  select(Category, Subcategory, hoursPerDayCombined, country_iso3) |> 
  left_join(country_regions, by = "country_iso3") |> 
  filter(country_name == "United Kingdom of Great Britain and Northern Ireland") |> 
  select(Category, Subcategory, hoursPerDayCombined)

# get plot data
uk_vor <- voronoi_treemap(hoursPerDayCombined ~ Category + Subcategory,
                          data = uk_data)
set.seed(1234)
groups <- filter(uk_vor, level == 1)
subgroups <- filter(uk_vor, level == 2) |> 
  group_by(group) |> 
  mutate(alpha = runif(1, 0, 0.6)) |> 
  ungroup()

# clock data
r <- 1.1
theta <- seq(0, (2 * pi), length.out = 13)[1:12]
clock_data <- tibble(
  x = r * cos(theta),
  y = r * sin(theta),
  angle = 90 + 360 * (theta / (2 * pi)),
  label = c("III", "II", "I", "XII", "XI", "X", "IX", "VIII", "VII", "VI", "V", "IV")
)
theta2 <- seq(0, (2 * pi), length.out = 61)[1:60]
clock_data2 <- tibble(
  x = r * cos(theta2),
  y = r * sin(theta2))
clock_data3 <- tibble(
  x = c(0.9, 0.7) * cos(theta[c(2, 6)]),
  y = c(0.9, 0.7) * sin(theta[c(2, 6)]),
  grp = c(1, 2)
)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-09-12", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define colours ----------------------------------------------------------

cols_vec = rcartocolor::carto_pal(length(unique(uk_data$Category))+1, "Prism")[1:length(unique(uk_data$Category))]
names(cols_vec) = unique(uk_data$Category)

bg_col <- "#fafafa"
text_col <- "grey10"
highlight_col <- cols_vec[1]


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "roboto"
)
title <- "How do we spend our time?"
st <- glue::glue("The daily activities of around 8 billion people occupy exactly 24 hours 
per day, placing a strict physical limit on what changes can be achieved in the 
world. These activities form the basis of human behavior, and the Human Chronome 
Project estimates how humans spend their time. In the United Kingdom of Great 
Britain and Northern Ireland, 
<span style='color:{cols_vec['Somatic maintenance']}'>**somatic maintenance**</span> activities such as sleeping 
take up the most time, closely followed by 
<span style='color:{cols_vec['Experience oriented']}'>**experience oriented**</span> activities such 
as social interaction.<br><br>
Other activities we spend our time on fall into the categories of 
<span style='color:{cols_vec['Food provision']}'>**food provision**</span>, 
<span style='color:{cols_vec['Nonfood provision']}'>**non-food provision**</span>, 
<span style='color:{cols_vec['Technosphere modification']}'>**technosphere modification**</span>, 
<span style='color:{cols_vec['Maintenance of surroundings']}'>**maintenance of surroundings**</span>, 
<span style='color:{cols_vec['Deliberate neural restructuring']}'>**deliberate neural restructuring**</span>, and
<span style='color:{cols_vec['Organization']}'>**organization**</span>.")
cap <- paste0(
  "**Data**: The Human Chronome Project<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  # voronoi diagram
  geom_polygon(data = groups,
               mapping = aes(x = x, y = y, group = group, fill = group),
               colour = bg_col,
               linewidth = 1) +
  geom_polygon(data = subgroups,
               mapping = aes(x = x, y = y, group = group, alpha = alpha),
               fill = bg_col,
               colour = bg_col,
               linewidth = 0.3) +
  # clock 
  geom_point(data = data.frame(),
             mapping = aes(x = 0, y = 0),
             size = 3,
             colour = text_col) +
  geom_segment(data = clock_data3,
             mapping = aes(x = 0, y = 0, xend = x, yend = y, group = grp),
             linewidth = 1,
             colour = text_col) +
  geom_point(data = clock_data2,
             mapping = aes(x = x, y = y),
             size = 0.5,
             colour = text_col) +
  geom_label(data = clock_data,
            mapping = aes(x = x, y = y, label = label),
            family = "robotoslab",
            size = 8,
            label.size = 0,
            fill = bg_col,
            colour = text_col,
            fontface = "bold") +
  scale_alpha_identity() +
  scale_fill_manual(values = cols_vec) +
  labs(title = title,
       tag = st,
       caption = cap) +
  coord_equal() +
  theme_void(base_size = 24, base_family = "roboto") +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(10, -110, 10, 110),
    plot.title = element_textbox_simple(
      colour = text_col,
      face = "bold",
      family = "robotoslab",
      lineheight = 0.5,
      size = 54,
      margin = margin(b = 20, t =10, l = -210)
    ),
    plot.tag = element_textbox_simple(
      colour = text_col,
      halign = 0,
      hjust = 0,
      maxwidth = 0.50,
      lineheight = 0.5,
      family = "roboto",
      margin = margin(b = 5, l = -125)
    ),
    plot.tag.position = c(-0.3, 0.5),
    plot.caption = element_textbox_simple(
      colour = text_col,
      lineheight = 0.5,
      family = "roboto",
      margin = margin(b = 5, l = -210)
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-09-12", paste0("20230912", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
