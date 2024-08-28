# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggHoriPlot)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-08-27")
power_rangers_episodes <- tuesdata$power_rangers_episodes
power_rangers_seasons <- tuesdata$power_rangers_seasons


# Load fonts --------------------------------------------------------------

font_add_google("Space Grotesk", "space")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "#472c85"
highlight_col <- "#d392b3"
col_palette <- c("#c2321c", "#da872c", "#e8dc3b", "#61b94b", "#2c3bd9", "#472c85")

body_font <- "space"
title_font <- "space"


# Data wrangling ----------------------------------------------------------

# Prep episode data
episode_data <- power_rangers_episodes |>
  left_join(power_rangers_seasons, by = "season_title") |>
  select(season_num, episode_num, IMDB_rating.x) |>
  complete(season_num, episode_num) |>
  mutate(
    season_num = factor(season_num, levels = 1:27)
  ) |> 
  mutate(
    season_num = fct_recode(season_num, "Season 1" = "1")
  )

# Horizon plot cutpoints
cutpoints <- episode_data |>
  mutate(
    outlier = between(
      IMDB_rating.x,
      quantile(IMDB_rating.x, 0.25, na.rm = TRUE) -
        1.5 * IQR(IMDB_rating.x, na.rm = TRUE),
      quantile(IMDB_rating.x, 0.75, na.rm = TRUE) +
        1.5 * IQR(IMDB_rating.x, na.rm = TRUE)
    )
  ) |>
  filter(outlier)
ori <- sum(range(cutpoints$IMDB_rating.x)) / 2
sca <- seq(range(cutpoints$IMDB_rating.x)[1],
  range(cutpoints$IMDB_rating.x)[2],
  length.out = 7
)[-4]


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-08-27", "recording"),
  device = "png",
  width = 5,
  height = 7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  twitter = NA,
  mastodon = NA,
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "<span style='font-size:38pt;'>**The Power of Power Rangers**</span><br>"
st <- "In 1993, five ordinary teenagers exploded on the pop-culture scene with
the launch of Mighty Morphin Power Rangers. Together they broke down barriers.
They defeated evil by demonstrating teamwork, inclusivity, and diversity to
people of all ages. Although newer seasons have fewer episodes, they are amongst
the highest rated episodes of all time."
cap <- paste0(
  title, st, "<br>**Data**: Kaggle<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(data = episode_data) +
  geom_horizon(
    mapping = aes(
      x = episode_num,
      y = IMDB_rating.x,
      fill = after_stat(Cutpoints)
    ),
    origin = ori, horizonscale = sca
  ) +
  facet_wrap(season_num ~ ., strip.position = "left", ncol = 1) +
  scale_fill_manual(
    values = rev(col_palette),
    name = "Lower IMDb Rating \u2190   \u2192 Higher IMDb Rating"
  ) +
  guides(
    fill = guide_legend(nrow = 1, reverse = TRUE)
  ) +
  labs(
    tag = cap
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 19, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag.position = c(0.72, 0.135),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.70, 0.31),
    legend.direction = "vertical",
    legend.text = element_blank(),
    legend.title = element_text(
      colour = text_col, hjust = 0.5,
      size = rel(1.3)
    ),
    legend.key.spacing = unit(0.3, "lines"),
    strip.text = element_text(
      angle = 0, colour = text_col,
      size = rel(1.2),
      hjust = 1,
      margin = margin(r = 2)
    ),
    panel.spacing = unit(0.1, "lines"),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.75,
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-08-27", paste0("20240827", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

ggsave(
  file.path("2024", "2024-08-27", paste0("20240827", ".png")),
  width = 5,
  height = 7
)
