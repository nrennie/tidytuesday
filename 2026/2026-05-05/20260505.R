# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-05-05")
food_beverages <- tuesdata$food_beverages
textiles <- tuesdata$textiles
transport <- tuesdata$transport


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

plot_data <- transport |>
  select(Year, Ships_launched, Ships_weight) |>
  mutate(Weight_per_ship = Ships_weight / Ships_launched) |>
  pivot_longer(-Year) |>
  mutate(
    name_label = case_when(
      str_detect(name, "launch") ~ "**Ship production**<br>Number of ships launched",
      str_detect(name, "_weight") ~ "**Total capacity**<br>Tons of gross tonnage",
      str_detect(name, "_per") ~ "**Ship capacity**<br>Average tons of gross tonnage per ship"
    )
  ) |>
  mutate(
    spec = (Year >= 1967) & str_detect(name, "launch", negate = TRUE)
  )

text_data <- plot_data |>
  group_by(name) |>
  slice_max(value) |>
  ungroup() |>
  arrange(Year, desc(value))

text_data2 <- data.frame(
  x = c(1945, 1967), 
  y = c(410, 700000),
  hjust = c(1, 1),
  name = c("Ships_launched", "Ships_weight"),
  txt = c("Production spikes after World War II",
          "Ships under 100 tons excluded after 1967")
)

plot_data$name <- factor(plot_data$name,
  levels = text_data$name,
  labels = text_data$name_label
)
text_data2$name <- factor(text_data2$name,
  levels = text_data$name,
  labels = text_data$name_label
)
text_data$name <- factor(text_data$name,
  levels = text_data$name,
  labels = text_data$name_label
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Fewer ships, but higher capacity"
st <- "Production volumes of transport equipment industry. Italy. 1861 - 1985."
cap <- paste0("**Note**: Non-nationalised rubber boats for port and strand service are excluded. Ships with gross tonnage lower than 100 tons have been excluded since 1967.<br>", source_caption(source = "Istituto nazionale di statistica", graphic = social))


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = Year, y = value, fill = spec,
                colour = spec)
) +
  geom_area(
    fill = "white", alpha = 1
  ) +
  geom_area(
    alpha = 0.4
  ) +
  geom_textbox(
    data = text_data |>
      mutate(
        x = c(1870, 1970, 1970),
        y = c(820, 1000000, 12000),
        txt = c(
          "Ship production peaked in ",
          "Total capacity peaked in ",
          "Average capacity also peaked in "
        ),
        hjust = c(0, 1, 1)
      ),
    mapping = aes(
      x = x, y = y,
      label = paste0(txt, Year),
      hjust = hjust,
      halign = hjust
    ),
    colour = text_col,
    family = body_font,
    fill = "transparent",
    box.colour = "transparent",
    size = 3,
    maxwidth = unit(6, "lines")
  ) +
  geom_textbox(
    data = text_data2,
    mapping = aes(
      x = x, y = y,
      label = txt,
      hjust = hjust,
      halign = hjust
    ),
    colour = text_col,
    family = body_font,
    fill = "transparent",
    box.colour = "transparent",
    size = 3,
    maxwidth = unit(5, "lines")
  ) +
  geom_line() +
  facet_wrap(~name, nrow = 1, scales = "free_y") +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    caption = cap,
    title = title,
    subtitle = st,
    x = NULL, y = NULL
  ) +
  scale_fill_manual(
    values = c(highlight_col, "grey60")
  ) +
  scale_colour_manual(
    values = c(highlight_col, "grey60")
  ) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 15, 5, 5),
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
    strip.text = element_textbox_simple(
      margin = margin(t = 10),
      size = rel(0.9),
      height = NULL,
      minheight = unit(3, "lines")
    ),
    panel.grid.minor = element_blank()
  ) +
  canvas(
    width = 7, height = 5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p

# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-05-05", paste0("20260505", ".png"))
)
