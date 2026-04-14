# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggiraph)
library(cowplot)
library(grid)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-04-14")
beaufort_scale <- tuesdata$beaufort_scale
birds <- tuesdata$birds
sea_states <- tuesdata$sea_states
ships <- tuesdata$ships


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

plot_data <- birds |>
  filter(str_detect(species_common_name, "penguin")) |>
  mutate(
    species_common_name = str_remove(species_common_name, " AD"),
    species_common_name = str_remove(species_common_name, " SUBAD")
  ) |>
  select(record_id, species_common_name, count) |>
  drop_na() |> 
  group_by(species_common_name) |> 
  mutate(n_tot = n()) |> 
  ungroup() |> 
  filter(n_tot > 1) |> 
  select(-n_tot) |> 
  mutate(id = row_number()) 

ship_data <- ships |> 
  filter(record_id %in% unique(plot_data$record_id)) |> 
  select(record_id, date, time, latitude, longitude) 

final_data <- plot_data |> 
  left_join(ship_data, by = "record_id") |> 
  mutate(time = format(strptime(time, "%H:%M:%S"), "%H:%M")) |> 
  mutate(label = glue("{count} penguin{if_else(count > 1, 's', '')} observed<br><b>Time</b>: {time} on {str_squish(format(date, '%B %e, %Y'))}<br><b>Location</b>: ({latitude}, {longitude})")) |> 
  arrange(desc(count))


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Little penguins are most common type of penguin in New Zealand"
st <- "Log book entries recorded by hand detail bird sightings at sea near New Zealand, from 1969 to 1990. The data was recorded using guidelines for the Australasian Seabird Mapping Scheme and counts seabirds seen from a ship during a 10 minute period. Of all penguins recorded, Little Penguins are the most common."
cap <- source_caption(source = "Museum of New Zealand Te Papa Tongarewa", graphic = social)


# Plot --------------------------------------------------------------------

ggplot(
  data = final_data,
  mapping = aes(x = date, y = 1, fill = species_common_name)
) +
  geom_point_interactive(
    mapping = aes(tooltip = label, data_id = id, size = count),
    position = position_jitter(width = 0, height = 0.2,  seed = 1404),
    colour = bg_col,
    alpha = 0.7,
    pch = 21
  ) +
  scale_size(range = c(3, 8)) +
  scale_x_date(limits = ymd(c("1969-01-01", "1990-12-31"))) +
  scale_y_continuous(limits = c(0.25, 1.75)) +
  scale_fill_manual(values= c("darkorange","purple","cyan4", "#ED1D5B")) +
  facet_wrap(~str_wrap(species_common_name, 20), ncol = 1) +
  labs(x = NULL,
       y = NULL,
       title = title,
       subtitle = st,
       caption = cap) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = body_font, base_size = 11.5) +
  theme(
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
      colour = alpha(text_col, 0.7),
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
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
      family = title_font,
      hjust = 0,
      size = rel(1.1)
    ),
    axis.title.x = element_text(
      hjust = 1, colour = alpha(text_col, 0.7),
      margin = margin(r = -5, t = 3)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    axis.text.y = element_blank(),
    legend.position = "none"
  ) -> g

# Static
g_int <- ggdraw(g) +
  draw_text(
    x = 0.6, y = 0.7,
    size = 11,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("Each circle represents a log book entry.", 17)
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.56, y1 = 0.73,
      x2 = 0.49, y2 = 0.68,
      curvature = 0.3,
      gp = gpar(col = text_col, lwd = 1.5, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_text(
    x = 0.45, y = 0.4,
    size = 11,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("Bigger circles represents more penguins observed.", 22)
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.63, y1 = 0.42,
      x2 = 0.70, y2 = 0.38,
      curvature = -0.35,
      gp = gpar(col = text_col, lwd = 1.5, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) 

# Static
g_int +
  canvas(
    width = 7, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p

# Interactive
girafe(
  ggobj = g_int,
  width_svg = 7,
  height_svg = 7,
  bg = bg_col,
  options = list(
    opts_tooltip(
      css = glue("
        padding: 5pt;
        font-family: {body_font};
        font-size: 1rem;
        background-color: {bg_col};
        border-radius: 3px;
        line-height: 1;
        color: {text_col};")
    ),
    opts_hover(css = glue("opacity:1;")),
    opts_hover_inv(css = "opacity:0.3;"),
    opts_toolbar(saveaspng = FALSE, hidden = "saveaspng"),
    opts_zoom(max = 1)
  )
)


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-04-14", paste0("20260414_static", ".png"))
)
