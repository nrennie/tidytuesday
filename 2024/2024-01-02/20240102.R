# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(statebins)
library(ggtext)
library(nrBrand)
library(glue)
library(gh)


# Load data ---------------------------------------------------------------

# Check it works
gh_gql("query { viewer { login }}")

# Get number of contributions by day
contributions <- 'query {
  user(login: "nrennie") {
    contributionsCollection(from: "2023-01-01T00:00:00Z", to: "2023-12-31T23:59:59Z") {
      contributionCalendar {
        weeks {
          contributionDays {
            date
            contributionCount
            color
          }
        }
      }
    }
  }

}'
json_data <- gh_gql(contributions)

# Process output
plot_data <- json_data$data$user |>
  unlist() |>
  unname() |>
  matrix(ncol = 3, byrow = TRUE) |>
  as.data.frame() |>
  magrittr::set_colnames(c("date", "contributions", "colour")) |>
  as_tibble() |>
  mutate(
    contributions = as.numeric(contributions),
    date = ymd(date)
  ) |>
  mutate(
    weekday = wday(date),
    week = week(date)
  )

# Save to separate file
write_csv(plot_data, "2024/2024-01-02/plot_data.csv")
plot_data <- readr::read_csv("2024/2024-01-02/plot_data.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "ubuntu")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "black"
highlight_col <- slice_max(plot_data, contributions)$colour

body_font <- "ubuntu"
title_font <- "ubuntu"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-01-02", "recording"),
  device = "png",
  width = 10,
  height = 3,
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
title <- "GitHub contributions: nrennie"
st <- glue("{format(sum(plot_data$contributions), big.mark = ',')} contributions 
           in 2023 ")
cap <- paste0(
  "**Data**: GitHub<br>**Graphic**: ", social
)


# Legend data -------------------------------------------------------------

greens <- plot_data |> 
  arrange(contributions) |> 
  group_by(colour) |> 
  slice_head() |> 
  ungroup() |> 
  arrange(contributions) |> 
  pull(colour)

leg_data <- data.frame(
  week = 44:48,
  weekday = rep(9, 5),
  colour = greens
)

leg_text <- data.frame(
  x = c(41.5, 49),
  y = c(9, 9),
  label = c("Less", "More")
)

months_data <- plot_data |> 
  filter(mday(date) == 1) |> 
  pull(week)

month_labels <- data.frame(
  x = months_data,
  y = rep(0, 12),
  label = month.abb
)

  

# Plot --------------------------------------------------------------------

ggplot() +
  statebins:::geom_rtile(
    data = plot_data,
    mapping = aes(x = week, y = weekday, fill = colour),
    radius = grid::unit(3, "pt"),
    width = 0.9,
    height = 0.9
  ) +
  statebins:::geom_rtile(
    data = leg_data,
    mapping = aes(x = week, y = weekday, fill = colour),
    radius = grid::unit(3, "pt"),
    width = 0.9,
    height = 0.9
  ) +
  # Add legend labels
  geom_text(
    data = leg_text,
    mapping = aes(x = x, y = y, label = label),
    family = body_font,
    colour = text_col,
    size = 12,
    hjust = 0
  ) +
  # Add month labels
  geom_text(
    data = month_labels,
    mapping = aes(x = x, y = y, label = label),
    family = body_font,
    colour = text_col,
    size = 12,
    hjust = -0.5
  ) +
  scale_fill_identity() +
  scale_y_reverse(breaks = c(2, 4, 6),
                  labels = c("Mon", "Wed", "Fri")) +
  coord_fixed() +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  theme_void(base_size = 32, base_family = body_font) +
  theme(
    plot.margin = margin(10, -15, 10, 15),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.text.y = element_text(colour = text_col,
                               hjust = 0,
                               margin = margin(r = -20)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      size = rel(1.8),
      face = "bold",
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = -20),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-01-02", paste0("20240102", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
