
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(igraph)
library(ggraph)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-09-19")
cran_20230905 <- tuesdata$cran_20230905
package_authors <- tuesdata$package_authors
cran_graph_nodes <- tuesdata$cran_graph_nodes
cran_graph_edges <- tuesdata$cran_graph_edges


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "Roboto")
font_add_google("Roboto Slab", "Roboto Slab")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "#f2f2f2"
text_col <- "#000643"
highlight_col <- "#0365c0"


# Data wrangling ----------------------------------------------------------

# look only at packages that list ggplot2 as a dependency
ggplot2_pkgs <- cran_20230905 |> 
  filter(str_detect(Imports, "ggplot2") | str_detect(Depends, "ggplot2")) |> 
  pull(Package)
all_pkgs <- package_authors |> 
  filter(Package %in% ggplot2_pkgs) |> 
  pull(Package) |> 
  unique()
pkg_mat <- matrix(0, nrow = length(all_pkgs), ncol = length(all_pkgs))
colnames(pkg_mat) <- all_pkgs
rownames(pkg_mat) <- all_pkgs

# two packages are connected if they share an author
for (i in 1:length(all_pkgs)) {
  for (j in 1:length(all_pkgs)) {
    if (i < j) {
      auth_i <- package_authors[package_authors$Package == all_pkgs[i], ]$authorsR
      auth_j <- package_authors[package_authors$Package == all_pkgs[j], ]$authorsR
      common <- length(intersect(auth_i, auth_j))
      if (common != 0) {
        pkg_mat[i, j] <- common
        pkg_mat[j, i] <- common
      }
    }
  }
  print(paste0(i, "/", length(all_pkgs)))
}
saveRDS(pkg_mat, "2023/2023-09-19/pkg_mat.rds")
pkg_mat <- readRDS("2023/2023-09-19/pkg_mat.rds")

# reduce the list to only packages since 2020
# sort by date
new_ggplot2_pkgs <- cran_20230905 |> 
  filter(str_detect(Imports, "ggplot2") | str_detect(Depends, "ggplot2")) |>
  mutate(Date = lubridate::ymd(Date)) |> 
  filter(Date >= lubridate::ymd("20200101")) |> 
  arrange(Date) |> 
  pull(Package)
new_pkg_mat <- pkg_mat[new_ggplot2_pkgs, new_ggplot2_pkgs]

# create an igraph object
pkg_g <- igraph::graph_from_adjacency_matrix(new_pkg_mat,
                                             mode = "undirected",
                                             weighted = TRUE)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-09-19", "recording"),
  device = "png",
  width = 4,
  height = 5.5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "Roboto"
)
title <- "<br><span style='font-size: 48px; font-weight: bold; font-family:\"Roboto Slab\";'>The R Package Network</span><br><br>"
st <- "<span style='font-family:\"Roboto\";'>R packages are often considered 
connected if one is a dependency of the other. Alternatively, packages can be 
considered connected if they share a common author. This chart shows the links 
between packages through their authors - for packages that list {ggplot2} in 
package Imports or Depends, and have been updated on CRAN since Jan 1, 2020. 
Brighter lines indicate a stronger connection, with nodes ordered by publish 
date.</span><br><br>"
cap <- paste0(title, st,
  "**Data**: CRAN collaboration graph<br> **Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggraph(pkg_g, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(mapping = aes(colour = log10(weight)), edge_width = 0.1) + 
  coord_fixed() +
  labs(caption = cap) +
  scale_edge_color_gradient(low = highlight_col, high = "white") +
  theme_void(base_size = 24) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = text_col, colour = text_col),
        plot.margin = margin(t = -17),
        plot.caption = element_textbox_simple(
          colour = text_col,
          margin = margin(l = 10, t = 5),
          lineheight = 0.5,
          family = "Roboto"
        ))

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-09-19", paste0("20230919", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
