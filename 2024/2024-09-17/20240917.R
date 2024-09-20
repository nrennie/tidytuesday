# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-09-17")
hamlet <- tuesdata$hamlet
macbeth <- tuesdata$macbeth
romeo_juliet <- tuesdata$romeo_juliet


# Load fonts --------------------------------------------------------------

font_add_google("MedievalSharp")
showtext_auto()


# Data wrangling ----------------------------------------------------------

max_lines <- hamlet |> 
  filter(character != "[stage direction]") |> 
  count(act, scene) |> 
  pull(n) |> 
  max()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-09-17", "recording"),
  device = "png",
  width = 4,
  height = 8,
  units = "in",
  dpi = 300
)


# Plot --------------------------------------------------------------------

act_plot <- function(Act,
                     Scene,
                     bg_col = "#F2E8D9",
                     col_palette = c("#16425B", "#48A9A6", "#D4B483", "#C1666B"),
                     text_col = "#484541",
                     text_family = "MedievalSharp",
                     text_size = 3,
                     border = 0,
                     save = FALSE,
                     s = 1234) {
  
  # choose data
  play_data <- hamlet |> 
    filter(act == Act, scene == Scene) |> 
    filter(character != "[stage direction]") |> 
    pull(dialogue)
  
  # processing
  max_n <- max(nchar(play_data))
  play_data <- tolower(play_data)
  play_data <- stringr::str_pad(play_data, max_n, side = "right")
  play_data <- stringr::str_split_fixed(play_data, "", n = max_n)
  colnames(play_data) <- seq_len(max_n)
  play_data <- tibble::as_tibble(play_data)

  # set colours
  set.seed(s)
  unique_chars <- unique(unlist(play_data))
  unique_chars <- unique_chars[unique_chars %in% letters]
  plot_cols <- tibble::tibble(
    value = letters,
    fill_col = sample(
      grDevices::colorRampPalette(col_palette)(length(letters))
    )
  )

  # text
  social <- nrBrand::social_caption(
    bg_colour = bg_col,
    icon_colour = text_col,
    font_colour = text_col,
    font_family = text_family
  )
  title <- "HAMLET by WILLIAM SHAKESPEARE"
  subtitle <- glue("{Act}, {Scene}")
  cap <- paste0(
    "**Data**: shakespeare.mit.edu<br>**Graphic**:", social
  )

  # plot data
  plot_data <- play_data |>
    dplyr::mutate(y = dplyr::row_number(), .before = 1) |>
    tidyr::pivot_longer(
      cols = -y,
      names_to = "x",
      values_to = "value"
    ) |>
    dplyr::mutate(x = as.numeric(x)) |>
    dplyr::left_join(plot_cols, by = "value") |>
    dplyr::mutate(fill_col = tidyr::replace_na(fill_col, bg_col))

  # plot
  g <- ggplot() +
    geom_tile(
      data = plot_data,
      mapping = aes(x = x, y = y, fill = fill_col),
      height = 0.9,
      width = 0.8
    ) +
    geom_text(
      data = dplyr::filter(plot_data, !(value %in% letters)),
      mapping = aes(x = x, y = y, label = value),
      colour = text_col,
      size = text_size,
      family = text_family
    ) +
    scale_y_reverse() +
    #scale_y_reverse(limits = c(max_lines, 1)) +
    scale_fill_identity() +
    labs(
      title = title, 
      subtitle = subtitle,
      caption = cap) +
    theme_void(base_size = 20) +
    theme(
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      plot.margin = margin(border, border, border, border, unit = "in"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_textbox_simple(
        colour = text_col,
        hjust = 0.5,
        halign = 0.5,
        margin = margin(b = 0, t = 10),
        lineheight = 0.5,
        family = text_family,
        face = "bold"
      ),
      plot.subtitle = element_textbox_simple(
        colour = text_col,
        hjust = 0.5,
        halign = 0.5,
        margin = margin(b = -15, t = 5),
        lineheight = 0.5,
        family = text_family
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        hjust = 0.5,
        halign = 0.5,
        margin = margin(b = 10, t = -10),
        lineheight = 0.5,
        family = text_family
      ),
    )
  if (save) {
    fname <- glue("2024/2024-09-17/plots/{Act}_{Scene}.png")
    ggsave(fname, g, width = 4, height = 8)
  }
  return(g)
}


# Loop over scenes --------------------------------------------------------

# check it works
act_plot("Act I", "Scene I", save = TRUE)

# map over all acts/scenes
scenes <- hamlet |> 
  distinct(act, scene)

purrr::walk2(
  .x = scenes$act,
  .y = scenes$scene,
  .f = ~act_plot(.x, .y, save = TRUE)
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-09-17", paste0("20240917", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#F2E8D9"
)
