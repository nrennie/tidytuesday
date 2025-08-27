# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggh4x)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-08-26")
billboard <- tuesdata$billboard
topics <- tuesdata$topics


# Load fonts --------------------------------------------------------------

font_add_google("Noto Music", "music")
font_add_google("Parisienne", "paris")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#FDF9D8"
text_col <- "#2F2823"
highlight_col <- "#2F2823"

body_font <- "paris"
title_font <- "paris"


# Data wrangling ----------------------------------------------------------

plot_data <- billboard |>
  filter(cdr_genre == "Funk/Soul") |>
  select(date, songwriter_male, happiness) |>
  mutate(date = as.Date(date)) |>
  filter(year(date) >= 1960, year(date) <= 2020) |>
  mutate(
    songwriter_male =
      case_when(
        songwriter_male == 0 ~ "All female",
        songwriter_male == 1 ~ "All male",
        songwriter_male %in% c(2, 3) ~ "Mixed gender"
      )
  ) |>
  drop_na()


avg_lines <- data.frame(
  start = ymd(c(
    "19600101", "19750101",
    "19900101", "20050101"
  )),
  end = ymd(c(
    "19750101", "19900101",
    "20050101", "2020101"
  ))
) |>
  rowwise() |>
  mutate(
    score = median(
      filter(plot_data, date >= start, date < end)$happiness
    ),
    `All female` = median(
      filter(
        plot_data, date >= start, date < end,
        songwriter_male == "All female"
      )$happiness
    ),
    `All male` = median(
      filter(
        plot_data, date >= start, date < end,
        songwriter_male == "All male"
      )$happiness
    ),
    `Mixed gender` = median(
      filter(
        plot_data, date >= start, date < end,
        songwriter_male == "Mixed gender"
      )$happiness
    )
  ) |> 
  ungroup() |> 
  pivot_longer(
    cols = -c(start, end, score),
    names_to = "songwriter_male"
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-08-26", "recording"),
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
  font_family = body_font,
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
title <- "Who writes the happiest number one funk and soul songs?"
st <- "Using from the Billboard Hot 100 Number Ones Database, and Spotify's measure of song happiness, we can compare how happy songs by all female, all male, and mixed gender songwriting teams are. All male singwriting teams dominate, especially in the mid-1970s, and their songs were very happy."
cap <- paste0(
  st,
  "<br><br>**Data**: Billboard Hot 100 Number Ones Database | **Graphic**: ", social
)


# Plot --------------------------------------------------------------------

# prep annotations
a1 <- annotate("text",
  label = "𝄆", size = 16,
  family = "music",
  hjust = 0,
  x = ymd("19730601"), y = 36
)
a2 <- annotate("text",
  label = "𝄇", size = 16,
  family = "music",
  hjust = 0.7,
  x = ymd("20200101"), y = 36
)
a3 <- annotate("text",
  label = "𝄞", size = 15,
  family = "music",
  hjust = 0,
  x = ymd("19650102"), y = 35
)
a4 <- annotate("text",
  label = "𝄴", size = 13,
  family = "music",
  hjust = 0,
  x = ymd("19700102"), y = 36
)


# main plot
ggplot(
  data = plot_data,
  mapping = aes(x = date, y = happiness)
) +
  geom_text(
    mapping = aes(label = "𝅘𝅥𝅮"),
    family = "music"
  ) +
  facet_wrap(~songwriter_male,
    ncol = 1
  ) +
  scale_x_date(
    limits = ymd(c("19600101", "2020101")),
    breaks = ymd(c(
      "19600101", "19750101", "19900101",
      "20050101", "2020101"
    )),
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, length.out = 5),
    limits = c(0, 100)
  ) +
  labs(
    x = NULL, y = NULL,
    title = title, subtitle = cap
  ) +
  # annotations
  at_panel(a1, PANEL == 1) +
  at_panel(a2, PANEL == 1) +
  at_panel(a3, PANEL == 1) +
  at_panel(a4, PANEL == 1) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_family = body_font) +
  theme(
    plot.margin = margin(5, 15, 10, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    panel.grid.major.x = element_line(
      colour = text_col
    ),
    panel.grid.major.y = element_line(
      colour = text_col
    ),
    axis.text.y = element_text(size = rel(0.7)),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.8, "lines"),
    strip.text.x.top = element_text(
      hjust = 0,
      size = rel(1.1),
      margin = margin(t = 5, b = 5)
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-08-26", paste0("20250826", ".png")),
  height = 5,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-08-26", paste0("20250826", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
