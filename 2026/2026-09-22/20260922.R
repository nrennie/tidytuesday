# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(glue)
library(ggview)
library(emojifont)


# Load data ---------------------------------------------------------------

urban <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-22/urban.csv')

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
highlight_col <- "#519623"


# Data wrangling ----------------------------------------------------------

plot_data <- urban |>
  filter(countryOrTerritoryName == "United Kingdom of Great Britain and Northern Ireland") |>
  select(year,
         city = cityName,
         green_area = averageShareOfGreenAreaInCityUrbanAreaPct,
  ) |>
  drop_na() |>
  mutate(
    city = str_extract(city, "^.*?(?= \\(|$)"),
    city = str_remove(city, "-Bedworth")
  )

plot_2020 <- plot_data |>
  filter(year %in% c(2020, 2010)) |>
  pivot_wider(names_from = year, values_from = green_area) |>
  mutate(change = round(`2020` - `2010`)) |>
  mutate(
    label = case_when(
      change > 0 ~ fontawesome("fa-arrow-circle-up"),
      change < 0 ~ fontawesome("fa-arrow-circle-down"),
      TRUE ~ fontawesome("fa-circle-o")
    )
  ) |>
  select(city, `2020`, label) |>
  pivot_longer(-c(label, city),
               names_to = "year",
               values_to = "green_area") |>
  arrange(desc(green_area))

plot_data$city <- factor(plot_data$city,
                         levels = plot_2020$city
)
plot_2020$city <- factor(plot_2020$city,
                         levels = plot_2020$city
)

# Define text -------------------------------------------------------------

title <- glue('<span style="font-family:{title_font}; font-size:17pt;">**Green, green grass of Stoke**</span><br>Stoke-on-Trent  has dramatically increased the percentage of green space in the city since 1990, coming highest in 2020. Dundee remains at the bottom of the table, with even less green space than in previous years.')
st <- "Average share of green area in UK city urban areas. 1990 - 2020."
cap <- glue('**Note**: Icons in top right corners show change between 2020 and 2010. Data for other UK cities is not currently available.<br>**Source**: UN Habitat Urban Indicators Database<br>**Graphic**: <span style="font-family:fontawesome-webfont;">{fontawesome("fa-github")}</span> nrennie <span style="font-family:fontawesome-webfont;">{fontawesome("fa-gitlab")}</span> nrennie <span style="font-family:fontawesome-webfont;">{fontawesome("fa-linkedin")}</span> nicola-rennie')


# Plot --------------------------------------------------------------------

ggplot() +
  geom_rect(
    data = plot_data,
    mapping = aes(
      ymin = 0, ymax = 100,
      xmin = year - 5, xmax = year + 5
    ),
    fill = "grey80"
  ) +
  geom_rect(
    data = plot_data,
    mapping = aes(
      ymin = 0, ymax = green_area,
      xmin = year - 5, xmax = year + 5,
      fill = as.character(year)
    ),
  ) +
  # city names
  geom_text(
    data = plot_2020,
    mapping = aes(x = 1986, y = 95, label = city),
    vjust = 1,
    hjust = 0,
    family = title_font
  ) +
  # up down labels
  geom_text(
    data = plot_2020,
    mapping = aes(x = 2024, y = 95, label = label),
    vjust = 1,
    hjust = 1,
    family = "fontawesome-webfont",
  ) +
  # Percentage labels
  geom_text(
    data = plot_data,
    mapping = aes(x = year, y = green_area + 3,
                  label = paste0(round(green_area), "%")),
    vjust = 0,
    hjust = 0.5,
    size = 2.5,
    family = body_font
  ) +
  scale_fill_manual(
    values = c("#41781c", "#519623", "#41781c", "#519623")
  ) +
  scale_x_continuous(breaks = seq(1990, 2020, 10)) +
  labs(title = title,
       subtitle = st,
       caption = cap,
       x = NULL, y = NULL) +
  facet_wrap(~city, ncol = 4, axes = "all_x") +
  coord_cartesian(expand = FALSE) +
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
      family = body_font,
      size = rel(1)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font,
      size = rel(0.9)
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font,
      size = rel(0.9)
    ),
    strip.text = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = rel(0.9),
                               family = body_font),
    panel.spacing = unit(0.4, "lines")
  ) +
  canvas(
    width = 5, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-09-22", paste0("20260922", ".png"))
)
