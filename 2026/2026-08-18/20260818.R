# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(rnaturalearth)
library(ggiraph)
library(sf)


# Load data ---------------------------------------------------------------

demo_by_first_language <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_first_language.csv")
demo_by_nationality <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_nationality.csv")
demo_by_reasons <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_reasons.csv")
performance_by_first_language <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/performance_by_first_language.csv")
performance_by_nationality <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/performance_by_nationality.csv")


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
highlight_col <- "#7F055F"


# Data wrangling ----------------------------------------------------------

choose_type <- "General Training" # "Academic" or "General Training"

world <- ne_countries()

score_data <- performance_by_nationality |>
  mutate(type = str_replace(type, "_", " ")) |>
  filter(
    year == "2024-2025", type == choose_type
  ) |>
  select(nationality, part, score) |>
  mutate(part = str_to_sentence(part)) |>
  mutate(
    nationality = recode(nationality,
      "Korea, Republic of" = "South Korea",
      "Iran, Islamic Republic of" = "Iran",
      "Russian Federation" = "Russia"
    )
  )

map_data <-
  left_join(
    world,
    score_data,
    by = c("name_en" = "nationality")
  ) |>
  select(name_en, part, score, geometry) |>
  filter_out(name_en == "Antarctica")

label_data <- map_data |>
  st_drop_geometry() |>
  as_tibble() |>
  mutate(
    score_label = if_else(
      is.na(score),
      NA,
      glue("{part}: {round(score, 1)}")
    ),
    score_label = if_else(
      !is.na(score) & part == "Overall",
      glue("**{score_label}**"),
      score_label
    )
  ) |>
  select(-c(part, score)) |>
  group_by(name_en) |>
  mutate(score_label = str_flatten(score_label, collapse = "<br>")) |>
  ungroup() |>
  distinct() |>
  mutate(
    label = if_else(
      is.na(score_label),
      glue("<b>{name_en}</b><br><i>No data</i>"),
      glue("<b>{name_en}</b><br>{score_label}")
    )
  ) |>
  select(name_en, label)

plot_data <- map_data |>
  left_join(label_data, by = "name_en") |>
  filter(is.na(part) | part == "Overall") |>
  mutate(
    colour_label = case_when(
      score <= 3.5 ~ "Extremely limited",
      score > 3.5 & score <= 5.5 ~ "Limited / Modest",
      score > 5.5 & score <= 7.5 ~ "Competent / Good",
      score > 7.5 ~ "Very good / Expert"
    )
  )

col_palette <- rev(PrettyCols::prettycols("Coast")[c(1,2,4,5)])
names(col_palette) <- c(
  "Extremely limited",
  "Limited / Modest",
  "Competent / Good",
  "Very good / Expert"
)

# add legend at the bottom
# add numbers to scores


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)

highest_score <- score_data |>
  filter(part == "Overall") |>
  slice_max(score) |>
  pull(nationality)

if (highest_score == "United States of America") {
  highest_score <- paste0("the ", highest_score)
}

title <- glue("Test takers from {highest_score} have the highest average overall IELTS ({choose_type}) score")
st <- glue("Average score for {choose_type} International English Language Testing System (IELTS) exams by nationality of test taker. 2024-2025.")
cap <- paste0("**Note 1**: The IELTS consists of 4 parts: Listening, Speaking, Reading and Writing. Each part is scored using a 'band' system from 1 to 9, and then the four parts are averaged for an overall score.<br>**Note 2**: There are two versions of the exam: Academic (for candidates applying for higher education or professional registration), and General Training (for candidates seeking work visas, vocational training, or migration to English-speaking countries). The Reading and Writing parts of the test differ between the Academic and General Training versions.<br>", source_caption(source = "ILETS Research (ielts.org/", graphic = social))


# Plot --------------------------------------------------------------------

g_int <- ggplot() +
  geom_sf_interactive(
    data = plot_data,
    mapping = aes(
      fill = colour_label,
      data_id = name_en,
      tooltip = label
    ),
    colour = bg_col
  ) +
  scale_fill_manual(
    values = col_palette,
    na.value = "grey80"
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = ""
  ) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
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
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )


# Interactive -------------------------------------------------------------

girafe(
  ggobj = g_int,
  bg = bg_col,
  width_svg = 6,
  height_svg = 4,
  options = list(
    opts_tooltip(
      delay_mouseover = 500,
      opacity = 0.9,
      css = glue("
        padding: 5pt;
        font-family: {body_font};
        font-size: 1rem;
        background-color: {bg_col};
        color: {text_col};
        border: solid;
        border-color: {text_col};
        border-radius: 5px;
        border-width: 2px")
    ),
    opts_hover(css = "opacity: 1;"),
    opts_hover_inv(css = "opacity: 0.7;"),
    opts_toolbar(hidden = c("saveaspng", "fullscreen")),
    opts_zoom(max = 1)
  )
)


# Save --------------------------------------------------------------------

g_int +
  canvas(
    width = 6, height = 4,
    units = "in", bg = bg_col,
    dpi = 300
  ) # -> p

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-08-18", paste0("20260818", ".png"))
)

