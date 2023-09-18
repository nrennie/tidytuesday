
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(igraph)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-09-19")
cran_20230905 <- tuesdata$cran_20230905
package_authors <- tuesdata$package_authors
cran_graph_nodes <- tuesdata$cran_graph_nodes
cran_graph_edges <- tuesdata$cran_graph_edges

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


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
      split_authors <- package_authors |> 
        filter(Package %in% c(all_pkgs[i], all_pkgs[j])) |> 
        group_split(Package) 
      common <- length(intersect(split_authors[[1]]$authorsR, split_authors[[2]]$authorsR))
      pkg_mat[i, j] <- common
      pkg_mat[j, i] <- common
    }
  }
}
saveRDS(pkg_mat, "2023/2023-09-19/pkg_mat.rds")
pkg_mat <- readRDS("2023/2023-09-19/pkg_mat.rds")

# create an igraph object
pkg_g <- igraph::from_adjacency(pkg_mat)

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-09-19", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "roboto"
)
title <- ""
st <- ""
cap <- paste0(
  "**Data**: <br>", social
)


# Plot --------------------------------------------------------------------



# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-09-19", paste0("20230919", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
