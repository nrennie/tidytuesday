# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-10-28")
prizes <- tuesdata$prizes


# Load fonts --------------------------------------------------------------

font_add_google("Libre Franklin", "libre")
font_add_google("Domine", "domine")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "grey98"
text_col <- "black"
non_highlight_col <- "grey80"
highlight_col <- "#E08300"
highlight_col2 <- "#820263"

body_font <- "libre"
title_font <- "domine"


# Data wrangling ----------------------------------------------------------

prize_lookup <- prizes |>
  filter(
    person_role == "winner",
    prize_genre == "fiction"
  ) |>
  select(prize_id, prize_name) |>
  unique() |>
  arrange(prize_id) |>
  mutate(prize_name = case_when(
    prize_id == 3 ~ "Costa First Novel Award (formerly Whitbread First Novel Award)",
    prize_id == 4 ~ "Costa Novel Award (formerly Whitbread Novel Award)",
    prize_id == 8 ~ "Booker Prize (formerly Man Booker Prize)",
    TRUE ~ prize_name
  )) |>
  unique()

plot_data <- prizes |>
  filter(
    person_role == "winner",
    prize_genre == "fiction"
  ) |>
  select(prize_id, prize_year, first_name, last_name, gender) |>
  left_join(prize_lookup, by = "prize_id")

annotate_data <- plot_data |> 
  count(prize_name, gender == "man") |> 
  group_by(prize_name) |> 
  mutate(tot_n = sum(n)) |> 
  filter(!`gender == "man"`) |> 
  mutate(p = round(100 * n/tot_n),
         label = case_when(
           p == 36 ~ paste0("<span style='font-size: 12pt'>**", p,
                            "%**</span><br>of winners are women or non-binary"),
           TRUE ~ paste0("<span style='font-size: 12pt'>**", p,
                         "%**</span><br>of winners are women")
         )) |> 
  ungroup() |> 
  arrange(p) |> 
  mutate(prize_name = factor(prize_name), levels = prize_name)

plot_data$prize_name <- factor(plot_data$prize_name, levels = annotate_data$prize_name)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-10-28", "recording"),
  device = "png",
  width = 7,
  height = 8,
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
title <- "Invisible ink? Under-representation persists in British literary prizes"
st <- glue("<span style='color:{highlight_col}'>**Women**</span> and <span style='color:{highlight_col2}'>**non-binary**</span> writers have historically been under-represented in the five main British literary prizes for fiction between 1990 and 2022. The exception is the *Women's Prize for Fiction*. However, the *Costa (First) Novel Award* has also shown more balanced representation in recent years.")
cap <- paste0(
  "**Note**: Information about a writer’s gender identity from scholarly and public sources can be sensitive. The data provided here enables the study of broad patterns and is not intended as definitive.<br>",
  "**Data**: Post45 Data Collective<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_tile(
    data = plot_data,
    mapping = aes(x = prize_year, y = "1", fill = gender),
    width = 0.95
  ) +
  geom_textbox(
    data = annotate_data,
    mapping = aes(x = 2025.5, y = "1", label = label),
    family = body_font,
    colour = text_col,
    hjust = 0.5,
    halign = 0.5,
    fill = "transparent",
    box.colour = "transparent",
    maxwidth = unit(1, "in"),
    size = 3
  ) +
  facet_wrap(~prize_name, ncol = 1) +
  scale_fill_manual(
    values = c(non_highlight_col, highlight_col2, highlight_col)
  ) +
  scale_x_continuous(limits = c(1990, 2028),
                     breaks = c(1995, 2005, 2015)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE) +
  theme_grey(base_size = 12, base_family = body_font) +
  theme(
    plot.margin = margin(10, 15, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5, l = 9),
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5, l = 9),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10, l = 9),
      family = body_font
    ),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, 
                              face = "bold", 
                              size = rel(0.9),
                              family = title_font)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-10-28", paste0("20251028", ".png")),
  width = 7,
  height = 8,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-10-28", paste0("20251028", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
