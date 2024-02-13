# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggstream)
library(snakecase)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-02-13")
historical_spending <- tuesdata$historical_spending
gifts_age <- tuesdata$gifts_age
gifts_gender <- tuesdata$gifts_gender


# Load fonts --------------------------------------------------------------

font_add_google("Sacramento", "sacramento")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()


# Define colours and fonts-------------------------------------------------

col_palette <- monochromeR::generate_palette("#CC444B", "go_both_ways",
  n_colours = length(unique(plot_data$Category))
)
bg_col <- col_palette[1]
text_col <- col_palette[8]
highlight_col <- col_palette[5]

body_font <- "ubuntu"
title_font <- "sacramento"


# Data wrangling ----------------------------------------------------------

spending_data <- historical_spending |>
  select(-PercentCelebrating) |>
  rowwise() |>
  mutate(Other = PerPerson - sum(c_across(Candy:GiftCards))) |>
  mutate(across(-c(Year), ~ .x / PerPerson)) |>
  select(-c(PerPerson)) |>
  pivot_longer(-Year, names_to = "Category", values_to = "Amount") |>
  mutate(Category = to_mixed_case(Category, sep_out = " "))

cat_levels <- spending_data |>
  filter(Year == 2011) |>
  arrange(Amount) |>
  pull(Category)

plot_data <- spending_data |>
  mutate(Category = factor(Category, levels = cat_levels))

label_data <- plot_data |> 
  filter(Year == 2011) |> 
  arrange(desc(Category)) |> 
  mutate(y1 = cumsum(Amount)) |> 
  arrange(Category) |> 
  mutate(y2 = 1 - cumsum(Amount)) |> 
  rowwise() |> 
  mutate(y = mean(c(y1, y2))) |> 
  mutate(y = case_when(
    Category == "Flowers" ~ y,
    TRUE ~ y
  ))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-02-13", "recording"),
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
  font_family = body_font
)
title <- glue::glue("<span style='font-family:{body_font}; font-size: 32pt; font-weight: bold;'>**How do people spend (money on) Valentine's Day?**</span>")
st <- glue::glue("Valentine's Day spending has increased from an average of $103 
                 per person in 2010 to a high of $196 in 2020, according to the 
                 US National Retail Federation. However, the breakdown 
                 of what people spend that money on has remained relatively 
                 unchanged over the last decade.")
cap <- paste0(
  "**Graphic**: ", social
)
txt <- glue::glue("{title}<br>{st}<br>{cap}")


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = Year, y = Amount)
) +
  geom_stream(aes(fill = Category),
    bw = 0.8, extra_span = 0.25, type = "proportional"
  ) +
  geom_richtext(data = label_data,
            mapping = aes(x = Year - 0.8, y = y, label = Category),
            family = body_font,
            size = 9,
            colour = text_col,
            fill = bg_col,
            hjust = 0) +
  labs(tag = txt) +
  scale_fill_manual(values = rev(col_palette)) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 21) +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag.position = c(0.97, 0.09),
    legend.position = "none",
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 10, t = 5),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.90
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-02-13", paste0("20240213", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
