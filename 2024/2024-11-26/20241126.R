# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(geofacet)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-11-26")
cbp_resp <- tuesdata$cbp_resp
cbp_state <- tuesdata$cbp_state


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()
showtext_opts(dpi = 300)


# Data wrangling ----------------------------------------------------------

plot_data <- cbp_state |>
  mutate(date = ymd(glue("{fiscal_year}-{month_abbv}-01"))) |>
  group_by(date, state, demographic) |>
  summarise(n = sum(encounter_count)) |>
  ungroup() |>
  mutate(
    demographic = case_when(
      demographic == "Single Adults" ~ "single adult",
      demographic == "FMUA" ~ "individual in a family unit",
      demographic == "UC / Single Minors" ~ "unaccompanied minor",
      demographic == "Accompanied Minors" ~ "accompanied minor"
    )
  ) |>
  filter(
    !(state %in% c("GU", "MP", "PR", "VI"))
  ) |>
  mutate(
    state = factor(state, levels = geofacet::us_state_grid2$code)
  ) |>
  complete(
    date, state, demographic,
    fill = list(n = 0)
  ) |>
  mutate(
    demographic = factor(demographic, levels = c(
      "unaccompanied minor", "accompanied minor",
      "individual in a family unit", "single adult"
    ))
  )


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
col_palette <- PrettyCols::prettycols("Prism")[c(1, 2, 4, 5)]
names(col_palette) <- levels(plot_data$demographic)
highlight_col <- col_palette[1]

body_font <- "roboto"
title_font <- "robotoslab"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-11-26", "recording"),
  device = "png",
  width = 7,
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
title <- "U.S. Customs and Border Protection Encounters"
cap <- paste0(
  "**Data**: www\\.cbp\\.gov <br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

encounter_plot <- function(pos, label_y, st_text) {
  g <- ggplot(
    data = plot_data,
    mapping = aes(x = date, y = n, fill = demographic)
  ) +
    geom_area(position = pos) +
    geom_text(
      data = filter(plot_data, date == ymd("2020-01-01"), demographic == "single adult"),
      mapping = aes(
        x = mean(c(ymd("2020-01-01"), ymd("2024-12-01"))),
        y = label_y,
        label = state
      ),
      family = title_font
    ) +
    facet_geo(~state, grid = "us_state_grid2") +
    scale_fill_manual(
      values = col_palette
    ) +
    labs(
      title = title,
      subtitle = st_text,
      caption = cap
    ) +
    coord_cartesian(expand = FALSE) +
    theme_void(base_family = body_font, base_size = 9) +
    theme(
      legend.position = "none",
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(5, 5, 5, 5),
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = "grey80", colour = "grey80"),
      strip.text = element_blank(),
      strip.background = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      plot.title = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 5, t = 5),
        lineheight = 0.5,
        face = "bold",
        size = rel(1.6),
        family = title_font
      ),
      plot.subtitle = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 5, t = 5),
        lineheight = 0.5,
        family = body_font,
        maxwidth = 0.9
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 0, t = 5),
        lineheight = 0.5,
        family = body_font
      )
    )
  return(g)
}


# Save plots --------------------------------------------------------------

st <- glue("The U.S. Customs and Borders Protection record data on individuals encountered. Individuals are recorded as either an <span style='color:{col_palette[[1]]}'>**{names(col_palette[1])}**</span>, an <span style='color:{col_palette[[2]]}'>**{names(col_palette[2])}**</span>, an <span style='color:{col_palette[[3]]}'>**{names(col_palette[3])}**</span>, or a <span style='color:{col_palette[[4]]}'>**{names(col_palette[4])}**</span>. The charts below show the changing proportions of demographics of individuals between January 2020 and November 2024. Gray areas indicate no encounters were recorded.")
encounter_plot("fill", 0.5, st)
camcorder::record_polaroid()
ggsave(
  filename = file.path("2024", "2024-11-26", paste0("20241126", ".png")),
  height = 5,
  width = 7
)

y_lim <- plot_data |>
  group_by(date, state) |>
  summarise(n = sum(n)) |>
  ungroup() |>
  pull(n) |>
  max() * 0.5
st <- glue("The U.S. Customs and Borders Protection record data on individuals encountered. Individuals are recorded as either an <span style='color:{col_palette[[1]]}'>**{names(col_palette[1])}**</span>, an <span style='color:{col_palette[[2]]}'>**{names(col_palette[2])}**</span>, an <span style='color:{col_palette[[3]]}'>**{names(col_palette[3])}**</span>, or a <span style='color:{col_palette[[4]]}'>**{names(col_palette[4])}**</span>. The charts below show the changing numbers of individuals of different demographics between January 2020 and November 2024. Gray areas indicate no encounters were recorded.")
encounter_plot("stack", y_lim, st)
camcorder::record_polaroid()
ggsave(
  filename = file.path("2024", "2024-11-26", paste0("20241126_stack", ".png")),
  height = 5,
  width = 7
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-11-26", paste0("20241126", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
