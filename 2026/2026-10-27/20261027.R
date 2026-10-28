# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggbeeswarm)
library(countrycode)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-10-27")
ikea_places <- tuesdata$ikea_places
ikea_seating <- tuesdata$ikea_seating


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#151C28"
text_col <- "#F2F4F8"
highlight_col <- "#007EF5"


# Data wrangling ----------------------------------------------------------

item_lookup <- ikea_seating |>
  filter(language == "en") |>
  select(item_no, product_type, country) |>
  mutate(
    product_type = str_to_lower(product_type),
    country = str_to_upper(country),
    country_name = countrycode(country, "iso2c", "country.name")
  ) |>
  distinct() |>
  mutate(
    country_name = factor(country_name),
    country_name = fct_relevel(country_name, "United Kingdom", "United States")
  ) |>
  group_by(item_no) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(-c(country, country_name)) |>
  mutate(
    category = case_when(
      str_detect(product_type, "armchair|chair|recliner") & !str_detect(product_type, "slipcover|cushion|inflatable|child") ~ "1 seat ",
      str_detect(product_type, "(?<!,)2-seat|two-seat") ~ "2 seats ",
      str_detect(product_type, "(?<!,)3-seat|three-seat") ~ "3 seats ",
      str_detect(product_type, "(?<!,)4-seat|four-seat") ~ "4 seats ",
      str_detect(product_type, "(?<!,)5-seat|five-seat") ~ "5 seats ",
      TRUE ~ "Other "
    )
  )

plot_data <- ikea_seating |>
  select(price_usd, name, item_no, country) |>
  mutate(country_name = countrycode(country, "iso2c", "country.name")) |>
  left_join(item_lookup, by = "item_no") |>
  drop_na()

avg_country_prices <- plot_data |>
  group_by(country_name, category) |>
  summarise(avg_price = mean(price_usd)) |>
  ungroup()

avg_prices <- plot_data |>
  group_by(category) |>
  summarise(avg_price = mean(price_usd)) |>
  ungroup()

avg_diff <- avg_prices |>
  filter(category != "Other ") |>
  mutate(diff = c(NA, diff(avg_price))) |>
  pull(diff) |>
  mean(na.rm = TRUE)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "How much will an extra seat cost you?"
cap <- source_caption(source = "Product listings from ikea.com | Exchange rates from the fawazahmed0 currency API", graphic = social)


# Plot function -----------------------------------------------------------

plot_function <- function(chosen_country = "United Kingdom") {
  # Calculate text and data
  st <- glue("Price of IKEA seating sold in <span style='color:#FFDA1A'>**{chosen_country}**</span> compared to the <span style='color:{highlight_col};'>**global average**</span> for chairs and sofas with between 1 and 5 seats.")
  
  avg_country_diff <- avg_country_prices |>
    filter(
      country_name == chosen_country,
      category != "Other "
    ) |>
    mutate(diff = c(NA, diff(avg_price))) |>
    pull(diff) |>
    mean(na.rm = TRUE)
  
  # Plot
  g <- ggplot() +
    geom_quasirandom(
      data = filter(plot_data, country_name == chosen_country),
      mapping = aes(x = price_usd, y = "1"),
      colour = alpha("#FFDA1A", 0.3)
    ) +
    # Mean lines
    geom_vline(
      data = filter(avg_country_prices, country_name == chosen_country),
      mapping = aes(xintercept = avg_price),
      colour = bg_col,
      linewidth = 1
    ) +
    geom_vline(
      data = filter(avg_country_prices, country_name == chosen_country),
      mapping = aes(xintercept = avg_price),
      colour = "#FFDA1A",
      linewidth = 1,
      linetype = "41"
    ) +
    geom_vline(
      data = avg_prices,
      mapping = aes(xintercept = avg_price),
      colour = highlight_col,
      linewidth = 1
    ) +
    # Annotation
    geom_textbox(
      data = data.frame(
        x = 8000, y = "1", category = "1 seat ",
        label = glue("On average, an additional seat on a sofa costs the equivalent of **${round(avg_country_diff)}** in IKEA stores in {chosen_country}, compared to the average across all countries with IKEA stores of **${round(avg_diff)}**.")
      ),
      mapping = aes(x = x, y = y, label = label),
      colour = text_col,
      fill = alpha(bg_col, 0.1),
      box.colour = "transparent",
      hjust = 1,
      halign = 0,
      family = body_font,
      inherit.aes = FALSE,
      minwidth = unit(3.5, "in")
    ) +
    # Styling
    scale_x_continuous(
      label = scales::label_comma(),
      expand = expansion(0, 0),
      limits = c(0, 8000)
    ) +
    facet_wrap(~category, ncol = 1, strip.position = "left") +
    labs(
      title = title,
      subtitle = st,
      caption = cap,
      x = "Price (US$)", y = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 12, base_family = body_font) +
    theme(
      plot.margin = margin(5, 15, 5, 5),
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
        margin = margin(b = 5, t = 0),
        family = body_font
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 0, t = 10),
        family = body_font
      ),
      strip.text.y.left = element_text(
        face = "bold",
        angle = 0,
        margin = margin(t = 10, r = 3),
        size = rel(1.1),
        colour = text_col
      ),
      axis.title.x = element_text(
        hjust = 1, colour = alpha(text_col, 0.8)
      ),
      axis.text.x = element_text(colour = text_col, family = body_font),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(
        colour = alpha(text_col, 0.1)
      ),
      panel.grid.minor = element_blank(),
      panel.spacing.y = unit(1, "lines"),
      strip.clip = "off"
    )
  return(g)
}

# Save --------------------------------------------------------------------

# plot_data$country_name |> unique() |> sort()

plot_function("Mexico") +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p
save_ggplot(
  plot = p,
  file = file.path("2026", "2026-10-27", paste0("20261027", ".png"))
)

plot_function("Switzerland") +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p
save_ggplot(
  plot = p,
  file = file.path("2026", "2026-10-27", paste0("20261027_switzerland", ".png"))
)

plot_function("Thailand") +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p
save_ggplot(
  plot = p,
  file = file.path("2026", "2026-10-27", paste0("20261027_thailand", ".png"))
)

plot_function("Sweden") +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p
save_ggplot(
  plot = p,
  file = file.path("2026", "2026-10-27", paste0("20261027_sweden", ".png"))
)



