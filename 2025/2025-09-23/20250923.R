# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(cowplot)
library(grid)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-09-23")
fide_ratings_august <- tuesdata$fide_ratings_august
fide_ratings_september <- tuesdata$fide_ratings_september


# Load fonts --------------------------------------------------------------

font_add_google("Outfit")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "grey5"
text_col <- "white"
highlight_col <- "#A52ACB"

body_font <- "Outfit"
title_font <- "Outfit"


# Data wrangling ----------------------------------------------------------

plot_data <- fide_ratings_august |>
  mutate(age = 2025 - bday) |>
  select(title, age, sex) |>
  drop_na() |>
  mutate(
    sex = if_else(sex == "M", "Male", "Female")
  ) |>
  mutate(
    women_only = str_starts(title, "W"),
    title = str_remove(title, "W")
  ) |>
  mutate(
    title = factor(title,
      levels = c("GM", "IM", "FM", "CM"),
      labels = str_wrap(c(
        "Grandmaster", "International Master",
        "FIDE Master", "Candidate Master"
      ), 10)
    )
  ) |>
  mutate(
    ymin = case_when(
      sex == "Male" ~ 0.5,
      sex == "Female" & !women_only ~ 0.5,
      sex == "Female" & women_only ~ 0
    ),
    ymax = case_when(
      sex == "Male" ~ 1,
      sex == "Female" & !women_only ~ 1,
      sex == "Female" & women_only ~ 0.5
    )
  )

annotate_data <- plot_data |> 
  group_by(title, sex, women_only) |> 
  mutate(mean = median(age)) |> 
  ungroup() |> 
  select(-age) |> 
  distinct()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-09-23", "recording"),
  device = "png",
  width = 7,
  height = 7,
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
title <- "How old are titled chess players?"
st <- "Grandmaster is the highest title a chess player can attain, apart from World Champion. Once achieved, the title is held for life unless revoked for cheating. The title of Grandmaster, along with the lesser FIDE titles of International Master, FIDE Master, and Candidate Master, is open to all players regardless of gender. However, there is *Woman Grandmaster* title with lower requirements awarded only to women, alongside equivalents for lesser titles."
cap <- paste0(
  "**Data**: International Chess Federation<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

p <- ggplot(
  data = plot_data,
) +
  geom_rect(
    mapping = aes(
      xmin = age - 0.5, xmax = age + 0.5,
      ymin = ymin, ymax = ymax,
      fill = sex
    ),
    colour = "transparent",
    alpha = 0.08
  ) +
  geom_rect(
    data = annotate_data,
    mapping = aes(
      xmin = mean - 0.5, xmax = mean + 0.5,
      ymin = ymin, ymax = ymax
    ),
    fill = "white",
    colour = "transparent",
    alpha = 1
  ) +
  scale_fill_manual(
    values = c(highlight_col, "#2BB6A1")
  ) +
  scale_x_continuous(
    limits = c(0, 100)
  ) +
  facet_grid(title ~ sex, switch = "y") +
  labs(
    title = title, subtitle = st,
    caption = cap, x = "Age", y = NULL
  ) +
  coord_cartesian(expand = FALSE) +
  theme_grey(base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(
      fill = bg_col,
      colour = alpha(text_col, 0.5)
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      family = body_font
    ),
    axis.title.x = element_text(colour = text_col),
    axis.text.x = element_text(colour = text_col),
    axis.text.y = element_blank(),
    strip.text.x = element_text(
      hjust = 0, colour = text_col,
      face = "bold"
    ),
    strip.text.y.left = element_text(
      colour = text_col,
      angle = 0,
      hjust = 1,
      face = "bold"
    ),
    panel.spacing.x = unit(0.9, "lines"),
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks.y = element_blank()
  )
p

ggdraw(p) +
  draw_text(
    x = 0.62, y = 0.64,
    size = 12,
    colour = text_col,
    family = body_font,
    text = "All gender titles"
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.55, y1 = 0.67,
      x2 = 0.49, y2 = 0.73,
      curvature = 0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.1, "inches"))
    )
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.58, y1 = 0.67,
      x2 = 0.64, y2 = 0.73,
      curvature = -0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.1, "inches"))
    )
  ) +
  draw_text(
    x = 0.55, y = 0.53,
    size = 12,
    colour = text_col,
    family = body_font,
    text = "Women-only\ntitles"
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.55, y1 = 0.57,
      x2 = 0.49, y2 = 0.63,
      curvature = 0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.1, "inches"))
    )
  ) +
  draw_text(
    x = 0.42, y = 0.21,
    size = 12,
    colour = text_col,
    family = body_font,
    text = "Median age\nof title per gender"
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.33, y1 = 0.23,
      x2 = 0.28, y2 = 0.23,
      curvature = 0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.1, "inches"))
    )
  ) 


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-09-23", paste0("20250923", ".png")),
  width = 7,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-09-23", paste0("20250923", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
