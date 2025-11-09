# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggh4x)


# Load utils functions ----------------------------------------------------

source("utils.R")


# Parameters --------------------------------------------------------------

highlight_col <- "#1A7A89"


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-11-11")
who_tb_data <- tuesdata$who_tb_data


# Data wrangling ----------------------------------------------------------

who_tb_data |>
  select(year, e_inc_num)


world_data <- who_tb_data |>
  select(year, e_pop_num, e_inc_num, e_mort_num) |>
  group_by(year) |>
  summarise(
    total_pop = sum(e_pop_num),
    total_inc = sum(e_inc_num),
    total_mort = sum(e_mort_num, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    inc100k = 100000 * (total_inc / total_pop),
    mort100k = 100000 * (total_mort / total_pop)
  ) |>
  select(year, inc100k, mort100k) |>
  mutate(g_whoregion = "World")

region_data <- who_tb_data |>
  select(year, e_pop_num, e_inc_num, e_mort_num, g_whoregion) |>
  group_by(year, g_whoregion) |>
  summarise(
    total_pop = sum(e_pop_num),
    total_inc = sum(e_inc_num),
    total_mort = sum(e_mort_num, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    inc100k = 100000 * (total_inc / total_pop),
    mort100k = 100000 * (total_mort / total_pop)
  ) |>
  select(year, inc100k, mort100k, g_whoregion)

plot_data <- rbind(world_data, region_data) |>
  mutate(
    g_whoregion = factor(g_whoregion,
      levels = c(
        "World", "Africa", "South-East Asia",
        "Eastern Mediterranean", "Western Pacific",
        "Americas", "Europe"
      )
    )
  )

label_data <- plot_data |>
  filter(year == 2023) |>
  mutate(
    label_mort = case_when(
      g_whoregion == "World" ~ paste0(round(mort100k), "\ndeaths\nper\n100,000\npeople"),
      TRUE ~ paste0(" ", round(mort100k))
    ),
    label_inc = case_when(
      g_whoregion == "World" ~ paste0(round(inc100k), "\nnew cases\nper\n100,000\npeople"),
      TRUE ~ paste0(" ", round(inc100k))
    )
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-11-11", "recording"),
  device = "png",
  width = 6,
  height = 7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

st_inc <- round(sum(filter(who_tb_data, year == 2023)$e_inc_num) / 1000000, 1)
st_mort <- round(sum(filter(who_tb_data, year == 2023)$e_mort_num, na.rm = TRUE) / 1000000, 1)

title <- "TB cases are rising in the Western Pacific and South-East Asia"
st <- glue("Tuberculosis (TB) remains one of the world’s deadliest infectious diseases. WHO estimates that {st_inc} million people fell ill with TB in 2023, and {st_mort} million died from the disease. Monitoring TB burden is essential to guide national responses and global strategies. However, TB cases and mortality rates vary greatly between WHO regions, with some regions seeing a resurgence of TB cases in recent years.")
cap <- source_caption("World Health Organization")


# Facet layout ------------------------------------------------------------

design <- matrix(c(
  1, 1, 1,
  1, 1, 1,
  2, 3, 4,
  5, 6, 7
), 4, 3, byrow = TRUE)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  # Areas
  geom_area(
    mapping = aes(x = year, y = inc100k),
    alpha = 0.5,
    fill = highlight_col,
    colour = highlight_col
  ) +
  geom_area(
    mapping = aes(x = year, y = mort100k),
    alpha = 0.5,
    fill = highlight_col,
    colour = highlight_col
  ) +
  # End labels
  geom_point(
    data = label_data,
    mapping = aes(x = year, y = inc100k),
    colour = highlight_col,
  ) +
  geom_point(
    data = label_data,
    mapping = aes(x = year, y = mort100k),
    colour = highlight_col,
  ) +
  geom_text(
    data = label_data,
    mapping = aes(x = year + 0.3, y = inc100k, label = label_inc),
    colour = highlight_col,
    hjust = 0,
    vjust = 0,
    fontface = "bold",
    size = 3
  ) +
  geom_text(
    data = label_data,
    mapping = aes(x = year + 0.3, y = mort100k, label = label_mort),
    colour = highlight_col,
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 3
  ) +
  # Styling
  facet_manual(vars(g_whoregion), design = design) +
  labs(
    x = NULL, y = NULL, title = title, subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_tt() +
  theme(
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
      family = body_font,
      width = 1.11
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10, b = 5),
      size = rel(0.9)
    ),
    panel.background = element_blank(),
    panel.spacing.x = unit(1.3, "lines"),
    plot.margin = margin(5, 50, 5, 5)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-11-11", paste0("20251111", ".png")),
  width = 6,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-11-11", paste0("20251111", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
