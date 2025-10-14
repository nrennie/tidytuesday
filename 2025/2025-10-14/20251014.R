# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(countrycode)
library(WeightedTreemaps)
library(ggforce)
library(ggimage)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-10-14")
food_security <- tuesdata$food_security


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#5F1A37"
text_col <- "white"
highlight_col <- "#b1b5ae"

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------

pop_data <- world_bank_pop |> 
  filter(indicator == "SP.URB.TOTL") |> 
  select(country, `2017`) |> 
  rename(population = `2017`)

protein_data <- food_security |>
  filter(
    Item == "Average protein supply (g/cap/day) (3-year average)",
    Year_End == 2022
  ) |>
  select(Area, Value) |>
  mutate(
    Continent = countrycode(
      sourcevar = Area,
      origin = "country.name.en",
      destination = "continent"
    ),
    Code = countrycode(
      sourcevar = Area,
      origin = "country.name.en",
      destination = "iso3c"
    )
  ) |>
  drop_na() |> 
  left_join(pop_data, by = c("Code" = "country")) |> 
  mutate(
    Total = (Value * population) / 1000000
  ) |> 
  drop_na() |> 
  group_by(Code) |> 
  slice_head() |> 
  ungroup()

vor_data <- protein_data |> 
  mutate(Total = round(Total)) |> 
  select(Continent, Area, Total)
result <- voronoiTreemap(
  data = vor_data,
  levels = c("Continent", "Area"),
  cell_size = "Total",
  shape = "circle"
)
polygons <- WeightedTreemaps::get_polygons(result)
poly_df <- imap_dfr(polygons, ~ {
  if (is.null(.x)) return(NULL)
    coords <- sf::st_coordinates(.x)
    data.frame(
    x = coords[,1],
    y = coords[,2],
    id = .y
  )
})

set.seed(1234)
groups <- filter(poly_df, str_starts(id, "LEVEL1")) |> 
  mutate(id = str_remove(id, "LEVEL1_"))
subgroups <- filter(poly_df, str_starts(id, "LEVEL2")) |> 
  mutate(id = str_remove(id, "LEVEL2_")) |> 
  group_by(id) |>
  mutate(alpha = runif(1, 0, 0.5)) |>
  ungroup()

cols_vec <- rcartocolor::carto_pal(length(unique(protein_data$Continent)) + 1, "Pastel")[1:length(unique(protein_data$Continent))]
names(cols_vec) <- unique(protein_data$Continent)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-10-14", "recording"),
  device = "png",
  width = 7,
  height = 7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Asia has highest protein supply"
st <- glue("Data from the Food and Agriculture Organization of the United Nations on the average protein supply for the year ending 2022 shows the differences between 
<span style='color:{cols_vec[1]}'>**{names(cols_vec[1])}**</span>, 
<span style='color:{cols_vec[2]}'>**{names(cols_vec[2])}**</span>, 
<span style='color:{cols_vec[3]}'>**{names(cols_vec[3])}**</span>, 
<span style='color:{cols_vec[4]}'>**{names(cols_vec[4])}**</span>, and
<span style='color:{cols_vec[5]}'>**{names(cols_vec[5])}**</span>. Areas indicate the total protein supply per day for each country.")
cap <- paste0(
  "**Data**:  The Food and Agriculture Organization of the United Nations (FAO)<br>**Graphic**: ", social
)

annotation_1 <- protein_data |> 
  slice_max(Value)
label1 <- glue(
  "{annotation_1$Area} has the highest per capita protein supply with an average of {round(annotation_1$Value)}g per person per day.")

annotation_2 <- protein_data |> 
  slice_max(Total)
label2 <- glue(
  "{annotation_2$Area} has the highest total protein supply with an average of {formatC(round(1000*annotation_2$Total), format='d', big.mark=',')}kg per day.")

annotation_3 <- protein_data |> 
  slice_min(Value)
label3 <- glue(
  "{annotation_3$Area} has the lowest per capita protein supply with an average of {round(annotation_3$Value)}g per person per day.")

annotation_4 <- protein_data |> 
  slice_min(Total)
label4 <- glue(
  "{annotation_4$Area} has the lowest total protein supply with an average of just {formatC(round(1000*annotation_4$Total), format='d', big.mark=',')}kg per day.")


# Plot --------------------------------------------------------------------

ggplot() +
  # plate
  geom_circle(
    data = data.frame(x0 = 1000, y0 = 1000, r = 1100),
    mapping = aes(x0 = x0, y0 = y0, r = r),
    fill = highlight_col,
    colour = NA
  ) +
  # voronoi diagram
  geom_polygon(
    data = groups,
    mapping = aes(x = x, y = y, group = id, fill = id),
    colour = "white",
    linewidth = 1
  ) +
  geom_polygon(
    data = subgroups,
    mapping = aes(x = x, y = y, group = id, alpha = alpha),
    fill = "white",
    colour = "white",
    linewidth = 0.3
  ) +
  # cutlery
  geom_image(
    data = slice_head(protein_data, n = 1),
    aes(
      x = -500,
      y = 1000,
      image = "2025/2025-10-14/images/fork.png"
    ),
    size = 0.17
  ) +
  geom_image(
    data = slice_head(protein_data, n = 1),
    aes(
      x = 2500,
      y = 1000,
      image = "2025/2025-10-14/images/knife.png"
    ),
    size = 0.17
  ) +
  # annotations
  geom_textbox(
    data = data.frame(x = 2050, y = 2050, label = label4),
    mapping = aes(x = x, y = y, label = label),
    fill = "transparent",
    colour = "transparent",
    text.colour = text_col,
    family = body_font,
    size = 3.2,
    maxwidth = unit(1.2, "in")
  ) +
  geom_textbox(
    data = data.frame(x = 50, y = -50, label = label3),
    mapping = aes(x = x, y = y, label = label),
    fill = "transparent",
    colour = "transparent",
    text.colour = text_col,
    family = body_font,
    size = 3.2,
    maxwidth = unit(1.3, "in")
  ) +
  geom_textbox(
    data = data.frame(x = 1950, y = -200, label = label2),
    mapping = aes(x = x, y = y, label = label),
    fill = "transparent",
    colour = "transparent",
    text.colour = text_col,
    family = body_font,
    size = 3.2,
    maxwidth = unit(1.5, "in")
  ) +
  geom_textbox(
    data = data.frame(x = 100, y = 2100, label = label1),
    mapping = aes(x = x, y = y, label = label),
    fill = "transparent",
    colour = "transparent",
    text.colour = text_col,
    family = body_font,
    size = 3.2,
    maxwidth = unit(1.5, "in")
  ) +
  # arrows
  annotate("curve", x = 0, xend = 100, 
           y = 1900, yend = 1700,
           arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
           curvature = 0.2,
           colour = text_col) +
  annotate("curve", x = -200, xend = -125, 
           y = 300, yend = 800,
           arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
           curvature = -0.2,
           colour = text_col) +
  annotate("curve", x = 1700, xend = 1300, 
           y = 2200, yend = 2150,
           arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
           curvature = 0.2,
           colour = text_col) +
  annotate("curve", x = 2000, xend = 1900, 
           y = 50, yend = 200,
           arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
           curvature = 0.2,
           colour = text_col) +
  # styling
  scale_x_continuous(limits = c(-600, 2600)) +
  scale_y_continuous(limits = c(-300, 2200)) +
  scale_alpha_identity() +
  scale_fill_manual(values = cols_vec) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_fixed(clip = "off") +
  theme_void(base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      family = body_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 10),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 10),
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-10-14", paste0("20251014", ".png")),
  width = 7,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-10-14", paste0("20251014", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
