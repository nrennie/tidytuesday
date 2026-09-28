# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggforce)
library(emojifont)


# Functions ---------------------------------------------------------------

sample_circle <- function(category, n) {
  theta <- runif(n, 0, 2 * pi)
  r <- sqrt(runif(n, 0.05, 1))

  output <- tibble(
    category = category,
    x = r * cos(theta),
    y = r * sin(theta)
  )
  return(output)
}


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-09-29")
health <- tuesdata$health


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
highlight_col <- "#9F042E"


# Data wrangling ----------------------------------------------------------

avg <- "Mean"
if (avg == "Median") {
  dot_num <- 200
} else {
  dot_num <- 200
}

hospital_data <- health |>
  select(Income = GC_DEV_WIG_2025, Pop = HL_POP_HOS_2025) |>
  drop_na() |>
  group_by(Income) |>
  summarise(Pop = if_else(avg == "Median", median(Pop), mean(Pop))) |>
  mutate(
    n = round(Pop / dot_num),
    nice_pop = round(Pop / 1000),
    label = fontawesome("fa-hospital-o"),
    Income = str_to_sentence(Income),
    Income = if_else(
      str_detect(Income, "income"), Income, paste0(Income, " income")
    ),
    Income = factor(Income,
      levels = c(
        "Low income", "Lower middle income", "Upper middle income", "High income"
      )
    )
  ) |>
  arrange(Income) |>
  mutate(
    Income = factor(Income,
      levels = c(
        "Low income", "Lower middle income", "Upper middle income", "High income"
      ),
      labels = paste0(Income, ": ", nice_pop, ",000 people")
    )
  )

set.seed(20260929)
plot_data <- map(
  .x = 1:nrow(hospital_data),
  .f = ~ sample_circle(hospital_data$Income[.x], hospital_data$n[.x])
) |>
  bind_rows()


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Higher income countries tend to have lower population density around hospitals"
st <- paste0(avg, " population living within 1 km from a hospital in 2025 by World Bank income group.")
cap <- paste0("**Note**: Each dot represents approximately ", dot_num, " people.<br>", source_caption(source = "Global Human Settlement Urban Centre Database. Produced by the Joint Research Centre (JRC) of the European Commission.", graphic = social))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_circle(
    data = data.frame(x0 = 0, y0 = 0, r = 1),
    mapping = aes(x0 = x0, y0 = y0, r = r),
    fill = "grey90",
    colour = "grey70"
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(x = x, y = y),
    colour = bg_col,
    size = 2
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(x = x, y = y),
    colour = text_col,
    size = 2,
    alpha = 0.7
  ) +
  geom_text(
    data = hospital_data,
    mapping = aes(x = 0, y = 0, label = label),
    vjust = 0.5,
    hjust = 0.5,
    size = 7,
    colour = highlight_col,
    family = "fontawesome-webfont",
  ) +
  facet_wrap(~category) +
  coord_fixed() +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 0, 8, 0),
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
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.9),
      hjust = 0.5,
      halign = 0.5
    ),
    panel.grid.minor = element_blank()
  ) +
  canvas(
    width = 5, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-09-29", paste0("20260929", ".png"))
)

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-09-29", paste0("20260929_mean", ".png"))
)
