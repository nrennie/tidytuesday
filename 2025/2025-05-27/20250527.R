# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-05-27")
monsters <- tuesdata$monsters


# Functions ---------------------------------------------------------------

# define coords
# https://stackoverflow.com/questions/36579767/add-unit-labels-to-radar-plot-and-remove-outer-ring-ggplot2-spider-web-plot-co
coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") {
    "y"
  } else {
    "x"
  }

  # dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }

  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }

  ggproto("CordRadar", CoordPolar,
    theta = theta, r = r, start = start,
    direction = sign(direction),
    is_linear = function(coord) TRUE,
    render_bg = function(self, scale_details, theme) {
      scale_details <- rename_data(self, scale_details)

      theta <- if (length(scale_details$theta.major) > 0) {
        theta_rescale(self, scale_details$theta.major, scale_details)
      }
      thetamin <- if (length(scale_details$theta.minor) > 0) {
        theta_rescale(self, scale_details$theta.minor, scale_details)
      }
      thetafine <- seq(0, 2 * pi, length.out = 100)

      rfine <- c(r_rescale(self, scale_details$r.major, scale_details))

      # This gets the proper theme element for theta and r grid lines:
      #   panel.grid.major.x or .y
      majortheta <- paste("panel.grid.major.", self$theta, sep = "")
      minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
      majorr <- paste("panel.grid.major.", self$r, sep = "")

      ggplot2:::ggname("grill", grid::grobTree(
        ggplot2:::element_render(theme, "panel.background"),
        if (length(theta) > 0) {
          ggplot2:::element_render(
            theme, majortheta,
            name = "angle",
            x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
            y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
            id.lengths = rep(2, length(theta)),
            default.units = "native"
          )
        },
        if (length(thetamin) > 0) {
          ggplot2:::element_render(
            theme, minortheta,
            name = "angle",
            x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
            y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
            id.lengths = rep(2, length(thetamin)),
            default.units = "native"
          )
        },
        ggplot2:::element_render(
          theme, majorr,
          name = "radius",
          x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
          y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
          id.lengths = rep(length(thetafine), length(rfine)),
          default.units = "native"
        )
      ))
    }
  )
}


# Load fonts --------------------------------------------------------------

font_add_google("Jolly Lodger", "jolly")
font_add_google("Cabin", "cabin")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "gray5"
text_col <- "gray95"
highlight_col <- "#ABFF4F"

body_font <- "cabin"
title_font <- "jolly"


# Data wrangling ----------------------------------------------------------

plot_data <- monsters |>
  select(alignment, str, dex, con, int, wis, cha) |>
  filter(alignment != "Unaligned") |>
  mutate(alignment = if_else(
    alignment == "Neutral", "Neutral Neutral", alignment
  )) |>
  separate_wider_delim(alignment, names = c("Law", "Good"), delim = " ") |>
  mutate(
    Law = factor(Law, levels = c("Lawful", "Neutral", "Chaotic")),
    Good = factor(Good, levels = c("Good", "Neutral", "Evil")),
  ) |>
  group_by(Law, Good) |>
  summarise(
    n = n(),
    str = mean(str),
    dex = mean(dex),
    con = mean(con),
    int = mean(int),
    wis = mean(wis),
    cha = mean(cha)
  ) |>
  ungroup() |>
  pivot_longer(-c(Law, Good, n)) |>
  mutate(
    name = factor(name, levels = c(
      "str", "dex", "con", "int", "wis", "cha"
    ))
  )

legend_data <- monsters |>
  select(alignment, str, dex, con, int, wis, cha) |>
  filter(alignment == "Unaligned") |>
  summarise(
    n = n(),
    str = mean(str),
    dex = mean(dex),
    con = mean(con),
    int = mean(int),
    wis = mean(wis),
    cha = mean(cha)
  ) |>
  pivot_longer(-c(n)) |>
  mutate(alignment = "Unaligned") |>
  mutate(
    name = factor(name,
      levels = c(
        "str", "dex", "con", "int", "wis", "cha"
      ),
      labels = c(
        "Strength", "Dexterity", "Constitution",
        "Intelligence", "Wisdom", "Charisma"
      )
    )
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-05-27", "recording"),
  device = "png",
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Dungeons and Dragons Monsters"
st <- "In Dungeons & Dragons, *alignment* is a categorisation of the ethical and moral perspective of player characters, non-player characters, and creatures. In the fifth edition, creatures who attack innocents because they act on natural instinct and lack the intelligence to make moral decisions, are labelled as *unaligned*. Monsters with different alignments have different characteristics, with average scores shown below."
cap <- paste0(
  "**Data**: dndbeyond.com<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

main_plot <- ggplot(
  data = plot_data,
  mapping = aes(
    x = name,
    y = value,
    group = "1"
  )
) +
  # radar plot bg elements
  geom_line(
    data = data.frame(
      x = rep(levels(plot_data$name), 2),
      y = c(
        rep(0, length(levels(plot_data$name))),
        rep(20, length(levels(plot_data$name)))
      )
    ),
    mapping = aes(
      x = x,
      y = y,
      group = x
    ),
    colour = alpha(text_col, 0.3)
  ) +
  geom_point(
    data = data.frame(
      x = levels(plot_data$name),
      y = rep(20, length(levels(plot_data$name)))
    ),
    inherit.aes = FALSE,
    mapping = aes(
      x = x,
      y = y
    ),
    colour = "grey80",
    size = 1
  ) +
  # radar chart
  geom_polygon(
    fill = highlight_col,
    colour = highlight_col,
    alpha = 0.4
  ) +
  geom_point(colour = highlight_col, size = 1) +
  scale_y_continuous(
    limits = c(0, 22.5), expand = expansion(0, 0),
    breaks = c(0, 5, 10, 15, 20)
  ) +
  coord_radar() +
  facet_grid(
    Good ~ Law,
    switch = "y"
  ) +
  theme_minimal(base_size = 11, base_family = body_font) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha(text_col, 0.5)
    ),
    plot.margin = margin(5, 300, 5, 5),
    plot.background = element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    panel.background = element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    strip.text.y.left = element_text(
      angle = 0, face = "bold", colour = text_col
    ),
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(
      face = "bold", colour = text_col
    )
  )
main_plot

legend_plot <- ggplot(
  data = legend_data,
  mapping = aes(
    x = name,
    y = value,
    group = "1"
  )
) +
  # radar plot bg elements
  geom_line(
    data = data.frame(
      x = rep(levels(legend_data$name), 2),
      y = c(
        rep(0, length(levels(legend_data$name))),
        rep(20, length(levels(legend_data$name)))
      )
    ),
    mapping = aes(
      x = x,
      y = y,
      group = x
    ),
    colour = alpha(text_col, 0.3)
  ) +
  geom_point(
    data = data.frame(
      x = levels(legend_data$name),
      y = rep(20, length(levels(legend_data$name)))
    ),
    inherit.aes = FALSE,
    mapping = aes(
      x = x,
      y = y
    ),
    colour = "grey80",
    size = 2
  ) +
  # radar plot
  geom_polygon(
    fill = highlight_col,
    colour = highlight_col,
    alpha = 0.4
  ) +
  geom_point(colour = highlight_col, size = 2) +
  scale_y_continuous(
    limits = c(0, 21), expand = expansion(0, 0),
    breaks = c(0, 5, 10, 15, 20)
  ) +
  coord_radar() +
  facet_wrap(
    ~alignment
  ) +
  theme_minimal(base_size = 11, base_family = body_font) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      colour = text_col, size = rel(0.8)
    ),
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha(text_col, 0.5)
    ),
    panel.spacing = unit(0, "lines"),
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    strip.text = element_text(
      face = "bold", colour = text_col
    )
  )

main_plot +
  inset_element(legend_plot, 0.45, -0.2, 1, 1, clip = FALSE,
                align_to = "full") +
  plot_annotation(
    title = title,
    subtitle = st,
    caption = cap
  ) &
  theme(
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5, r = -200),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(2),
      maxwidth = 2
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5, r = -290),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 2
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-05-27", paste0("20250527", ".png")),
  height = 5,
  width = 8,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-05-27", paste0("20250527", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
