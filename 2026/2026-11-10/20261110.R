# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggiraph)
library(ggfx)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-11-10")
fiction_citations <- tuesdata$fiction_citations
fiction_deaths <- tuesdata$fiction_deaths
fiction_rankings <- tuesdata$fiction_rankings


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


# Data wrangling ----------------------------------------------------------

franhcise_lookup <- fiction_rankings |>
  select(franchise_slug, franchise) |>
  distinct()

plot_data <- fiction_deaths |>
  filter(franchise_slug == "star-wars") |>
  select(year, killer, victim, work, undone) |>
  arrange(year) |>
  mutate(
    label = glue("<b>{killer}</b> killed {victim}.<br><i>{work}. {year}.</i>")
  ) |>
  group_by(year, work) |>
  mutate(x = row_number(),
         y = cur_group_id()) |>
  ungroup() |> 
  mutate(
    killer_cat = if_else(
      killer %in% c("Darth Vader", "Anakin Skywalker"), killer, "other"
    )
  )


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Who Killed Whom in a Galaxy Far, Far Away?"
st <- "Cited Feats, is a fan-run site that ranks who is strongest inside 21 franchises, from Marvel and Star Wars to One Piece, Berserk, Westeros and Dungeons & Dragons. The site also records who killed whom, and whether the death was later undone. Within the *Star Wars* franchise, the most prolific killer was <span style='color:#4791FF;'>**Anakin Skywalker**</span>, closely followed by <span style='color:#FF3333;'>**Darth Vader**</span>. Three of the deaths were later <span style='color:#00F5AF;'>**undone**</span>."
cap <- paste0("**Note**: Each row represents a different released work, and each name within a row represents a different kill in that work.<br>", source_caption(source = "Cited Feats", graphic = social, sep = " | "))


# Plot --------------------------------------------------------------------

g <- ggplot() +
  # timeline
  geom_text(
    data = plot_data |> group_by(year) |> slice_head(n = 1) |> ungroup(),
    mapping = aes(y = y, x = 0.8, label = year),
    hjust = 1,
    colour = text_col
  ) +
  geom_line(
    data = plot_data |> group_by(year) |> slice_head(n = 1) |> ungroup(),
    mapping = aes(y = y, x = 0.9),
    colour = text_col
  ) +
  # year markers
  geom_point(
    data = plot_data |> group_by(year) |> slice_head(n = 1) |> ungroup(),
    mapping = aes(y = y, x = 0.9),
    size = 5,
    colour = text_col
  ) +
  geom_point_interactive(
    data = plot_data |> group_by(year, work) |> slice_head(n = 1) |> ungroup(),
    mapping = aes(y = y, x = 0.9, tooltip = work),
    size = 2,
    colour = text_col
  ) +
  # data
  geom_textbox(
    data = filter_out(plot_data, killer %in% c("Anakin Skywalker", "Darth Vader")),
    mapping = aes(y = y, x = x, label = killer, fill = undone, colour = undone),
    hjust = 0, halign = 0.5,
    box.size = 0.7,
    box.colour = "grey30",
    fontface = "bold",
    width = unit(1.5, "inch"),
    height = unit(0.2, "inch"),
    valign = 0.5
  ) +
  with_outer_glow(
    geom_textbox(
      data = filter(plot_data, killer == "Anakin Skywalker"),
      mapping = aes(y = y, x = x, label = killer, fill = undone, colour = undone),
      hjust = 0, halign = 0.5,
      box.size = 0.7,
      box.colour = text_col,
      fontface = "bold",
      width = unit(1.5, "inch"),
      height = unit(0.2, "inch"),
      valign = 0.5
    ),
    colour = "#2A7FFF",
    sigma = 5, #10,
    expand = 5 #10
  ) +
  with_outer_glow(
    geom_textbox(
      data = filter(plot_data, killer == "Darth Vader"),
      mapping = aes(y = y, x = x, label = killer, fill = undone, colour = undone),
      hjust = 0, halign = 0.5,
      box.size = 0.7,
      box.colour = text_col,
      fontface = "bold",
      width = unit(1.5, "inch"),
      height = unit(0.2, "inch"),
      valign = 0.5
    ),
    colour = "#FF0000",
    sigma = 5, #10,
    expand = 5 #10
  ) +
  # overlay
  geom_rect_interactive(
    data = plot_data,
    mapping = aes(xmin = x, xmax = x + 0.9,
                  ymin = y - 0.45, ymax = y + 0.45,
                  tooltip = label),
    fill = "transparent",
    colour = "transparent"
  ) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  scale_x_continuous(limits = c(0.5, max(plot_data$x) + 1), expand = expansion(0, 0)) +
  scale_y_reverse() +
  scale_fill_manual(
    values = c(
      "FALSE" = bg_col,
      "TRUE" = "#00A375"
    )
  ) +
  scale_colour_manual(
    values = c(
      "FALSE" = text_col,
      "TRUE" = bg_col
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 11, base_family = body_font) +
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
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 0),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

g +
  canvas(
    width = 6, height = 9,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-11-10", paste0("20261110", ".png"))
)


# Interactive -------------------------------------------------------------

girafe(
  ggobj = g,
  width_svg = 6,
  height_svg = 9,
  bg = bg_col,
  options = list(
    opts_tooltip(
      css = glue("
        padding: 5pt;
        font-family: {body_font};
        font-size: 2rem;
        background-color: {bg_col};
        border-radius: 3px;
        line-height: 1;
        color: {text_col};")
    ),
    opts_hover(css = glue("opacity:1;")),
    opts_hover_inv(css = "opacity:1;"),
    opts_toolbar(saveaspng = FALSE, hidden = "saveaspng"),
    opts_zoom(max = 1)
  )
)
