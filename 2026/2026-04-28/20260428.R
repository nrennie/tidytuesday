# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-04-28")
agreements <- tuesdata$agreements
quantity_codes <- tuesdata$quantity_codes
tariff_agricultural <- tuesdata$tariff_agricultural
tariff_codes <- tuesdata$tariff_codes


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

plot_data <- tariff_agricultural |>
  filter(agreement == "mfn", rate_type_code == 7) |>
  left_join(tariff_codes, by = "hts8", relationship = "many-to-many") |>
  select(description, begin_effective_date, end_effective_date, ad_val_rate) |>
  arrange(description, begin_effective_date) |>
  filter(
    str_detect(description, "jam"),
    str_detect(description, "nesoi", negate = TRUE)
  ) |>
  mutate(
    ad_val_rate = 100 * ad_val_rate,
    description = str_remove(description, ", nesi"),
    description = str_remove_all(description, "\\bjam\\b"),
    description = str_remove(description, "\\s*\\([^\\)]*\\)")
  )

last_data <- plot_data |>
  group_by(description) |>
  arrange(desc(ad_val_rate)) |>
  slice_max(begin_effective_date, with_ties = FALSE) |>
  ungroup() |>
  arrange(desc(ad_val_rate))

plot_data$description <- factor(
  plot_data$description,
  levels = last_data$description
)
last_data$description <- factor(
  last_data$description,
  levels = last_data$description
)


# Jam jars ----------------------------------------------------------------

# Jar
jar_body <- function(x0 = 0.20, y0 = 0.00, x1 = 0.80, y1 = 0.72, r = 0.02) {
  t <- seq(0, pi / 2, length.out = 30)
  bl <- data.frame(x = x0 + r - r * cos(t), y = y0 + r - r * sin(t))
  br <- data.frame(x = x1 - r + r * sin(t), y = y0 + r - r * cos(t))
  tr <- data.frame(x = x1 - r + r * cos(t), y = y1 - r + r * sin(t))
  tl <- data.frame(x = x0 + r - r * sin(t), y = y1 - r + r * cos(t))
  rbind(bl, br, tr, tl)
}
jar <- jar_body()

# Lid
lid <- data.frame(
  x = c(0.22, 0.78, 0.78, 0.22),
  y = c(0.72, 0.72, 0.80, 0.80)
)

# Cloth
cloth_top_x <- seq(0.22, 0.78, length.out = 300)
cloth_bot_x <- seq(0.12, 0.88, length.out = 300)
cloth_bot_y <- 0.62 + 0.03 * sin(seq(0, 3 * 2 * pi, length.out = 300))
cloth <- data.frame(
  x = c(cloth_top_x, rev(cloth_bot_x)),
  y = c(rep(0.72, 300), rev(cloth_bot_y))
)

# Label
label_rect <- data.frame(
  x = c(0.23, 0.77, 0.77, 0.23),
  y = c(0.40, 0.40, 0.56, 0.56)
)

rescale_jar <- function(df, x_range, y_range,
                        x_jar_min = 0.20, x_jar_max = 0.80) {
  x_num <- as.numeric(x_range)
  df$x <- as.Date(
    x_num[1] + (as.numeric(df$x) - x_jar_min) /
      (x_jar_max - x_jar_min) * diff(x_num),
    origin = "1970-01-01"
  )
  df$y <- y_range[1] + diff(y_range) * df$y
  df
}
x_range <- c(min(plot_data$begin_effective_date), today())
y_range <- c(0, max(plot_data$ad_val_rate) + 11)
jar <- rescale_jar(jar, x_range, y_range)
lid <- rescale_jar(lid, x_range, y_range)
cloth <- rescale_jar(cloth, x_range, y_range)
label_rect <- rescale_jar(label_rect, x_range, y_range)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Peach jam comes at a premium"
st <- "Most-Favored-Nation (MFN) tariff rates for imports into the United States from 1997-2025, for products using an *ad valorem* tariff, i.e., an import duty calculated as a fixed percentage of the total value of imported goods."
cap <- paste0(
  "**Note**: Citrus jams, fruit jellies, and marmalades do not include orange marmalades.<br>",
  source_caption(
    source = "USITC Tariff Database", graphic = social,
    sep = " | "
  )
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = jar, mapping = aes(x = x, y = y),
    fill = "grey93", colour = text_col
  ) +
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = begin_effective_date,
      xmax = pmin(end_effective_date, today()),
      ymin = 0, ymax = ad_val_rate,
      fill = description
    )
  ) +
  geom_polygon(
    data = jar, mapping = aes(x = x, y = y),
    fill = "transparent", colour = text_col,
    linewidth = 1
  ) +
  geom_polygon(
    data = cloth, mapping = aes(x = x, y = y),
    fill = "#FAF0E6", colour = text_col,
    linewidth = 1
  ) +
  geom_polygon(
    data = lid, mapping = aes(x = x, y = y),
    fill = "#FAF0E6", colour = text_col,
    linewidth = 1
  ) +
  geom_text(
    data = last_data,
    mapping = aes(
      x = today(), y = ad_val_rate,
      label = paste0(" ", ad_val_rate, "%")
    ),
    hjust = 0, vjust = 1,
    family = body_font,
    colour = text_col
  ) +
  geom_polygon(
    data = label_rect, mapping = aes(x = x, y = y),
    fill = "#FAF0E6", colour = text_col
  ) +
  geom_text(
    data = last_data,
    mapping = aes(
      x = mean(c(min(plot_data$begin_effective_date), today())),
      y = 8.7,
      label = str_wrap(description, 24),
      size = if_else(
        nchar(as.character(description)) > 20,
        3.2, 5
      )
    ),
    family = title_font,
    colour = text_col
  ) +
  scale_fill_manual(
    values = c(
      "#FFA07A", "#4d061c", "#C0D725", "#FEE12B",
      "#F19035", "#FA5053", "#CE4458", "#2E183B"
    )
  ) +
  scale_size_identity() +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  facet_wrap(~description, ncol = 4) +
  coord_cartesian(clip = "off") +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
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
    strip.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(0.5, "lines")
  ) +
  canvas(
    width = 8, height = 5.5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-04-28", paste0("20260428", ".png"))
)
