# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(funspotr)
library(png)
library(grid)


# Load data ---------------------------------------------------------------

r_pkgs <-
  list_files_github_repo("nrennie/tidytuesday",
    branch = "main"
  ) |>
  filter(stringr::str_detect(relative_paths, "2023")) |>
  spot_funs_files(
    show_each_use = TRUE,
    keep_in_multiple_pkgs = TRUE
  ) |>
  unnest_results()
write.csv(r_pkgs, "2024/2024-07-09/r_pkgs.csv", row.names = FALSE)

r_pkgs <- readr::read_csv("2024/2024-07-09/r_pkgs.csv")


# Load fonts --------------------------------------------------------------

font_add_google(name = "Source Code Pro", family = "source")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#3A3B3C"
text_col <- "white"
col_palette <- rcartocolor::carto_pal(9, "Bold")[1:8]
highlight_col <- col_palette[1]

body_font <- "source"
title_font <- "source"


# Data wrangling ----------------------------------------------------------

core_tidyverse <- c(
  "dplyr", "forcats", "ggplot2", "purrr",
  "readr", "stringr", "tibble", "tidyr"
)
core_lvls <- glue::glue(
  "&nbsp;&nbsp;&nbsp;&nbsp;<img src='2024/2024-07-09/hex/{core_tidyverse}.png' width='20'>")

plot_data <- r_pkgs |>
  mutate(
    pkgs = case_when(
      pkgs == "ggplot" ~ "ggplot2",
      TRUE ~ pkgs
    )
  ) |>
  filter(pkgs %in% core_tidyverse) |>
  separate_wider_delim(
    relative_paths,
    delim = "/",
    names = c(NA, "date", NA)
  ) |>
  mutate(date = lubridate::ymd(date)) |>
  group_by(date, pkgs) |>
  count() |>
  ungroup() |>
  mutate(
    pkgs = glue::glue("&nbsp;&nbsp;&nbsp;&nbsp;<img src='2024/2024-07-09/hex/{pkgs}.png' width='20'>")
  ) |> 
  mutate(pkgs = factor(pkgs, levels = core_lvls))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-07-09", "recording"),
  device = "png",
  width = 8,
  height = 4,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "How many times did I use {tidyverse} packages for TidyTuesday?"
st <- "The tidyverse is a collection of open source R packages that *share
an underlying design philosophy, grammar, and data structures of tidy data*.
Of the 8 core tidyverse packages {ggplot2} was my most used package for TidyTuesday
visualisations in 2023."
cap <- paste0(
  st, "<br>**Data**: github.com/nrennie/tidytuesday<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(plot_data) +
  geom_area(
    mapping = aes(
      x = date,
      y = n,
      fill = pkgs
    )
  ) +
  facet_wrap(~pkgs, nrow = 1, drop = FALSE) +
  scale_x_date(
    limits = c(ymd(20230101), ymd(20231231)),
    breaks = c(ymd(20230101), ymd(20230701)),
    labels = c("Jan", "Jul")
  ) +
  scale_y_continuous(
    limits = c(0, 150),
    breaks = c(0, 50, 100, 150)
  ) +
  coord_cartesian(expand = F) +
  scale_fill_manual(
    values = col_palette
  ) +
  labs(
    title = title, 
    subtitle = cap,
    x = "",
    y = "Number of package calls") +
  theme_gray(
    base_size = 22, base_family = body_font
  ) +
  theme(
    legend.position = "none", 
    plot.margin = margin(5, 10, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    strip.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 10),
      lineheight = 0.5,
      face = "bold",
      size = rel(1.9),
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    panel.spacing = unit(0.5, "lines"),
    strip.text.x = element_textbox_simple(
      color = text_col, face = "bold", hjust = 0.5, halign = 0.5),
    axis.text = element_text(colour = text_col),
    axis.title = element_text(colour = text_col),
    panel.grid.major = element_line(
      colour = alpha(text_col, 0.2),
      linewidth = 0.2),
    panel.grid.minor = element_line(
      colour = alpha(text_col, 0.3),
      linewidth = 0.2),
    axis.ticks = element_blank()
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-07-09", paste0("20240709", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
