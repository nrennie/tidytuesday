# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(countrycode)
library(lemon)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-12-24")
global_holidays <- tuesdata$global_holidays
monthly_passengers <- tuesdata$monthly_passengers


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Carter One", "carter")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
neg_col <- "#F4743B"
pos_col <- "#592E83"

body_font <- "roboto"
title_font <- "carter"


# Data wrangling ----------------------------------------------------------

flights_data <- monthly_passengers |>
  mutate(
    Date = ymd(glue("{Year}/{Month}/01"))
  ) |>
  mutate(
    Ratio = International / Domestic,
    Diff = Total - Total_OS
  ) |>
  mutate(
    Country = countrycode(ISO3,
      origin = "iso3c",
      destination = "country.name"
    ),
    Continent = countrycode(ISO3,
      origin = "iso3c",
      destination = "continent"
    )
  ) 

Countries <- flights_data |>
  drop_na() |>
  filter(Diff > 0) |>
  count(Country, sort = TRUE) |>
  pull(Country) 

plot_data <- flights_data |>
  drop_na() |>
  filter(Country %in% Countries) |>
  select(Date, Country, Diff) |>
  mutate(
    PosNeg = case_when(
      Diff > 0 ~ "Positive",
      Diff < 0 ~ "Negative"
    ),
  ) |> 
  filter(Diff != 0) |> 
  mutate(Diff = 1000 * Diff) |> 
  mutate(
    logDiff = case_when(
      PosNeg == "Negative" ~ -1 * log(abs(Diff + 1)),
      TRUE ~ log(abs(Diff + 1))
    )
  )

country_levels <- plot_data |> 
  group_by(Country) |> 
  summarise(x = max(abs(Diff + 1))) |> 
  arrange(desc(x)) |> 
  pull(Country)

plot_data$Country <- factor(plot_data$Country, levels = country_levels)

miss_data <- flights_data |>
  filter(Country %in% Countries) |> 
  filter(is.na(Diff)) |> 
  select(Country, Date)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-12-24", "recording"),
  device = "png",
  width = 5,
  height = 7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = neg_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "How accurate is open source flight data?"
st <- glue("For some countries, the total number of air passengers (in thousands) can be obtained from official statistics. The same information can also be obtained from other openly available data sources. These visualisations compare discrepancies between the two, highlighting where <span style='color:{neg_col}'>**open source estimates exceed official statistics**</span> or where <span style='color:{pos_col}'>**official statistics exceed open source estimates**</span>. China shows the largest difference between the two, with a discrepancy of alomost 40,000,000 passengers in a single month. Grey areas highlight time periods where comparisons could not be made due to missing data.")
cap <- paste0(
  st,
  "<br><br>**Data**: Lai S., Sorichetta A. and WorldPop (2020)<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_col(
    data = miss_data,
    mapping = aes(
      x = factor(Date),
      y = log(50000000)
    ),
    width = 1,
    alpha = 0.6,
    fill = "grey60"
  ) +
  geom_col(
    data = plot_data,
    mapping = aes(
      x = factor(Date),
      y = logDiff,
      fill = PosNeg
    ),
    width = 1,
    alpha = 0.7
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Country, ncol = 3) +
  scale_fill_manual(
    values = c(
      "Positive" = pos_col,
      "Negative" = neg_col
    ),
    guide = "none"
  ) +
  scale_x_discrete(
    breaks = factor(c(ymd("2010-01-01"), ymd("2017-01-01"))),
    labels = c("2010", "2017")
  ) +
  scale_y_symmetric(
    breaks = c(-log(50000000), -log(50000), 0, log(50000), log(50000000)),
    labels = c("-50,000,000", "-50,000", "0", "50,000", "50,000,000")
  ) +
  labs(
    title = title, subtitle = cap,
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 7) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.spacing = unit(0.3, "lines"),
    panel.background = element_rect(
      fill = bg_col,
      colour = text_col
    ),
    plot.title.position = "plot",
    strip.text = element_text(
      family = title_font, margin = margin(b = 2),
      size = rel(1.4)
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.x = element_text(size = rel(0.7), margin = margin(t = 2)),
    axis.text.y = element_text(hjust = 1, margin = margin(r = 2)),
    axis.ticks.y = element_line(linewidth = 4, colour = text_col)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2024", "2024-12-24", paste0("20241224", ".png")),
  width = 5,
  height = 7
)

gg_playback(
  name = file.path("2024", "2024-12-24", paste0("20241224", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
