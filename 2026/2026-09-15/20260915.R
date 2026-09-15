# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(WeightedTreemaps)
library(ggiraph)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-09-15")
dead_sea_scrolls <- tuesdata$dead_sea_scrolls


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"


# Data wrangling ----------------------------------------------------------

dss_data <- dead_sea_scrolls |>
  select(manuscript_id, site, site_parent, num_images) |>
  filter(num_images > 0)

result <- voronoiTreemap(
  data = dss_data,
  levels = c("site_parent", "site", "manuscript_id"),
  cell_size = "num_images",
  shape = "circle"
)
polygons <- WeightedTreemaps::get_polygons(result)
poly_df <- imap_dfr(polygons, ~ {
  if (is.null(.x)) {
    return(NULL)
  }
  coords <- sf::st_coordinates(.x)
  data.frame(
    x = coords[, 1],
    y = coords[, 2],
    id = .y
  )
})

set.seed(1234)
groups <- filter(poly_df, str_starts(id, "LEVEL1")) |>
  mutate(id = str_remove(id, "LEVEL1_"))
subgroups <- filter(poly_df, str_starts(id, "LEVEL2")) |>
  mutate(id = str_remove(id, "LEVEL2_")) 
subgroups2 <- filter(poly_df, str_starts(id, "LEVEL3")) |>
  mutate(id = str_remove(id, "LEVEL3_")) 

tooltips <- dead_sea_scrolls |> 
  mutate(tooltip = glue("<b>{composition}</b><br>{site}<br>Number of images: {num_images}")) |> 
  select(manuscript_id, tooltip)

plot_data <- subgroups2 |> 
  as_tibble() |> 
  left_join(
    tooltips, by = c("id" = "manuscript_id")
  )
  

# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
title <- "**Around 90% of the Dead Sea Scrolls were found in the Qumran Caves**"
st <- "The Dead Sea Scrolls are a set of ancient Jewish biblical manuscripts from the Second Temple period. They are seen by many as the most significant archaeological manuscript discovery of the 20<sup>th</sup> century. Most of the manuscripts were found at the Qumran Caves near Ein Feshkha in the West Bank, on the northern shore of the Dead Sea."
cap <- source_caption(source = "The Leon Levy Dead Sea Scrolls Digital Library. Israel Antiquities Authority.", graphic = social)
note_txt <- "**Note**: The archive catalogs 809 manuscripts from the collection of 981 assembled by researchers. Each area in the chart represents the number of high-resolution photographic plates of a manuscript in the IAA digital archive, serving as a proxy for size and preservation state. Six manuscrpts are not included in the chart as there are no recorded images of them."
tag_txt <- glue(
  "<span style='font-size:17pt;font-family:{title_font};'>{title}</span><br>{st}<br><br><span style='font-size:9pt;'>{cap}<br>{note_txt}</span>"
)


# Plot --------------------------------------------------------------------

g_int <- ggplot() +
  # voronoi diagram
  geom_polygon(
    data = groups,
    mapping = aes(x = x, y = y, group = id, fill = id),
    colour = "white",
    linewidth = 1
  ) +
  geom_polygon(
    data = subgroups,
    mapping = aes(x = x, y = y, group = id),
    fill = "transparent",
    colour = "white",
    linewidth = 0.3
  ) +
  geom_polygon_interactive(
    data = plot_data,
    mapping = aes(x = x, y = y, group = id, data_id = id, tooltip = tooltip),
    fill = "white",
    colour = "white",
    alpha = 0.2,
    linewidth = 0.05
  ) +
  # area labels
  geom_textbox(
    data = data.frame(
      site_parent = unique(dss_data$site_parent),
      x = c(750, 1000, 2050, 1800),
      y = c(1000, -100, 1000, 350),
      hjust = c(0.5, 0, 0, 0),
      size = c(8, 4, 4, 4)
      ),
    mapping = aes(x = x, y = y, label = site_parent,
                  hjust = hjust, halign = hjust,
                  colour = site_parent,
                  size = size),
    family = body_font,
    fontface = "bold",
    fill = "transparent",
    box.colour = "transparent",
    maxwidth = unit(3, "cm")
  ) +
  scale_colour_manual(
    values = c(bg_col, PrettyCols::prettycols("Dark")[2:4]),
    breaks = unique(dss_data$site_parent)
  ) +
  scale_fill_manual(
    values = PrettyCols::prettycols("Dark")[1:4],
    breaks = unique(dss_data$site_parent)
  ) +
  scale_x_continuous(limits = c(-100, 2500)) +
  scale_alpha_identity() +
  scale_size_identity() +
  labs(tag = tag_txt) +
  coord_fixed() +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 0, 5, 220),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      valign = 1,
      vjust = 1,
      margin = margin(b = 5, t = 5),
      family = body_font,
      maxwidth = 0.64
    ),
    plot.tag.position = c(-0.59, 1.09)
  )

g_int +
  canvas(
    width = 8, height = 5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-09-15", paste0("20260915", ".png"))
)


# Interactive -------------------------------------------------------------

girafe(
  ggobj = g_int,
  bg = bg_col,
  width_svg = 8,
  height_svg = 5,
  options = list(
    opts_tooltip(
      delay_mouseover = 500,
      opacity = 0.9,
      css = glue("
        padding: 5pt;
        font-family: {body_font};
        font-size: 1.5rem;
        background-color: {bg_col};
        color: {text_col};
        border: solid;
        border-color: {text_col};
        border-radius: 5px;
        border-width: 2px")
    ),
    opts_hover(css = "opacity: 0;"),
    opts_hover_inv(css = "opacity: 0.9;"),
    opts_toolbar(hidden = c("saveaspng", "fullscreen")),
    opts_zoom(max = 1)
  )
)

