# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(PrettyCols)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-01-28")
water_insecurity_2022 <- tuesdata$water_insecurity_2022
water_insecurity_2023 <- tuesdata$water_insecurity_2023


# Load fonts --------------------------------------------------------------

font_add_google("Dosis", "dosis")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#D7DFEA"
text_col <- "#0E131B"
highlight_col <- "#476085"

body_font <- "dosis"
title_font <- "dosis"


# Data wrangling ----------------------------------------------------------

water_insecurity <- rbind(
  water_insecurity_2022,
  water_insecurity_2023
) |>
  separate_wider_delim(name, names = c("county", "state"), delim = ", ")

plot_data <- water_insecurity |>
  mutate(
    n = percent_lacking_plumbing / 100 * total_pop
  ) |>
  group_by(state, year) |>
  summarise(
    total_pop = sum(total_pop),
    n = sum(n)
  ) |>
  ungroup() |>
  mutate(percent_lacking_plumbing = 100 * n / total_pop) |>
  select(-c(total_pop, n)) |>
  pivot_wider(
    names_from = year,
    values_from = percent_lacking_plumbing
  ) |>
  mutate(change_num = `2023` - `2022`) |>
  arrange(desc(change_num)) |>
  mutate(y = row_number()) |> 
  mutate(
    change = case_when(
      change_num > 0 ~ "increase",
      change_num == 0 ~ "no change",
      change_num < 0 ~ "decrease"
    )
  )

label_data <- plot_data |> 
  count(change)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-01-28", "recording"),
  device = "png",
  width = 6,
  height = 6,
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
title <- glue("<span style='font-size:17pt; font-weight:bold; font-family:{title_font};'>**Inequalities in water insecurity in the USA**</span><br>")
st <- glue("<br>Water insecurity can be influenced by number of social vulnerability indicators—from demographic characteristics to living conditions and socioeconomic status —that vary spatially across the U.S. Between 2022 and 2023, {filter(label_data, change == 'decrease')$n} states or regions saw a decrease in the percentage of the population lacking plumbing facilities. In contrast, {filter(label_data, change == 'increase')$n} saw an increase. Georgia, Lousiana, and Texas are not included as data were not available for both years.")
cap <- paste0(
  title, st, "<br>**Data**: tidycensus<br>**Graphic**:", social
)
txt <- "**Alaska** had the biggest increase in the percentage of the population without plumbing. The **Arctic Research Consortium of the United States** found that many homes without running water and wastewater services are non-servicable - they cannot be provided with plumbing services for reasons including homes that are not structurally sound or do not have a thermostatically controlled heat source."


# Plot --------------------------------------------------------------------

ggplot(
  data = drop_na(plot_data)
) +
  geom_col(
    mapping = aes(x = change_num, y = factor(y), fill = change_num),
    colour = text_col
  ) +
  geom_textbox(
    mapping = aes(
      x = change_num, y = factor(y), label = state,
      hjust = case_when(
        change_num >= 0 ~ 0,
        TRUE ~ 1
      ),
      halign = case_when(
        change_num >= 0 ~ 0,
        TRUE ~ 1
      ),
      vjust = 0.55, 
      valign = 0.55
    ),
    colour = text_col,
    box.colour = "transparent",
    fill = "transparent",
    family = body_font,
    size = 3
  ) +
  geom_textbox(
    data = data.frame(
      x = 0.12, y = 11, label = txt
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    fill = alpha(text_col, 0.05),
    family = body_font,
    size = 3.7,
    minwidth = 0.46
  ) +
  scale_x_continuous(
    limits = c(-0.23, 0.23),
    expand = expansion(0, 0)
  ) +
  scale_y_discrete(limits = rev) +
  scale_fill_pretty_div(
    palette = "RedBlues",
    direction = -1
  )  +
  labs(
    tag = cap,
    x = "Percentage point change in population lacking plumbing between 2022 and 2023",
    y = NULL
  ) +
  theme_minimal(base_family = body_font, base_size = 9.5) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    plot.tag.position = c(0, 1),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      vjust = 1,
      valign = 1,
      margin = margin(b = 5, t = 0),
      lineheight = 0.7,
      family = body_font,
      maxwidth = 0.46
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-01-28", paste0("20250128", ".png")),
  height = 6,
  width = 6,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-01-28", paste0("20250128", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
