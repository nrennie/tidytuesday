# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggimage)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-01-14")
conf2023 <- tuesdata$conf2023
conf2024 <- tuesdata$conf2024


# Load fonts --------------------------------------------------------------

font_add_google("Open Sans", "open")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "grey10"
text_col <- "white"
highlight_col <- "#ee6331"

body_font <- "open"


# Data wrangling ----------------------------------------------------------

reg_data <- conf2023 |>
  mutate(
    posit = case_when(
      str_detect(speaker_affiliation, "posit|Posit") ~ "posit",
      TRUE ~ "other"
    )
  ) |>
  select(posit, session_type) |>
  filter(session_type == "regular") |>
  arrange(desc(posit))
reg_data$x <- rep(1:10, times = 10)[1:nrow(reg_data)]
reg_data$y <- rep(1:10, each = 10)[1:nrow(reg_data)]

key_data <- conf2023 |>
  mutate(
    posit = case_when(
      str_detect(speaker_affiliation, "posit|Posit") ~ "oosit",
      TRUE ~ "other"
    )
  ) |>
  select(posit, session_type) |>
  filter(session_type == "keynote") |>
  arrange(desc(posit))
key_data$x <- rep(1:10, times = 10)[1:nrow(key_data)]
key_data$y <- rep(1:10, each = 10)[1:nrow(key_data)]

light_data <- conf2023 |>
  mutate(
    posit = case_when(
      str_detect(speaker_affiliation, "posit|Posit") ~ "posit",
      TRUE ~ "other"
    )
  ) |>
  select(posit, session_type) |>
  filter(session_type == "lightning") |>
  arrange(desc(posit))
light_data$x <- rep(1:10, times = 10)[1:nrow(light_data)]
light_data$y <- rep(1:10, each = 10)[1:nrow(light_data)]

plot_data <- do.call(rbind, list(key_data, reg_data, light_data)) |>
  mutate(
    image = glue("2025/2025-01-14/{posit}.png")
  ) |> 
  mutate(
    session_type = paste0(str_to_sentence(session_type), " Talks")
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-01-14", "recording"),
  device = "png",
  width = 7,
  height = 2.5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
st <- glue("<span style='font-size:18pt; font-weight:bold;'>posit::conf(2023)</span><br><br>After a rebanding from RStudio to Posit, 2023 saw the first posit::conf(2023) hosted in Chicago. The conference showcased advancements in open-source tools, data analysis, and reporting. The talks featured speakers from many industries, including several from <span style='color:{highlight_col};'>**Posit**</span> itself. The number of talks in each category from speakers with an affiliation to Posit is highlighted in a white hexagon below.")
cap <- paste0(
  st, "<br><br>**Data**: posit |**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = x, y = y)
) +
  geom_image(
    mapping = aes(image = image),
    size = 0.08
  ) +
  facet_wrap(~session_type, strip.position = "bottom") +
  labs(
    tag = cap
  ) +
  coord_fixed() +
  theme_void(base_family = body_font, base_size = 6) +
  theme(
    plot.margin = margin(10, 10, 0, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 1.1,
      family = body_font,
      maxwidth = 0.7
    ),
    strip.text.x.bottom = element_text(
      margin = margin(t = 3, b = 5),
      face = "bold",
      size = rel(1.2),
      colour = text_col
    ),
    panel.spacing = unit(1, "lines"),
    plot.tag.position = c(0.01, 0.72)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-01-14", paste0("20250114", ".png")),
  height = 2.5,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-01-14", paste0("20250114", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
