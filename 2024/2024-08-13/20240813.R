# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(lubridate)
library(spiralize)
library(ggimage)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-08-13")
worlds_fairs <- tuesdata$worlds_fairs


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Data wrangling ----------------------------------------------------------

fairs_data <- worlds_fairs |>
  mutate(
    start_date = dmy(paste0("01-", start_month, "-", start_year)),
    end_date = dmy(paste0("01-", end_month, "-", end_year))
  ) |>
  select(country, start_date, end_date) |>
  mutate(
    start_date_num = as.numeric(start_date, origin = as.Date("1850-01-01")),
    end_date_num = as.numeric(end_date, origin = as.Date("1850-01-01"))
  ) |>
  mutate(
    country = case_when(
      country %in% c("United States", "France", "Italy", "Belgium") ~ country,
      TRUE ~ "other"
    )
  )

col_palette <- c("#414288", "#DB2955", "#679436", "#FFE156", "#7D98A1")
names(col_palette) <- c("United States", "France", "Italy", "Belgium", "other")

plot_data <- left_join(
  fairs_data, enframe(col_palette),
  by = c("country" = "name")
)


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "grey30"
highlight_col <- col_palette[2]

body_font <- "roboto"
title_font <- "roboto_slab"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-08-13", "recording"),
  device = "png",
  width = 8,
  height = 4,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  twitter = NA
)
title <- "World's Fairs"
st <- glue(
  "A world's fair, also known as a universal exhibition or an expo, is a large global 
  exhibition designed to showcase the achievements of nations. These exhibitions vary 
  in character and are held in different parts of the world at a specific site for a 
  period of time, typically between three and six months. World's Fairs have been hosted 
  in the <span style='color:{col_palette[[1]]}'>{names(col_palette)[[1]]}</span>, 
  <span style='color:{col_palette[[2]]}'>{names(col_palette)[[2]]}</span>, 
  <span style='color:{col_palette[[3]]}'>{names(col_palette)[[3]]}</span>, 
  <span style='color:{col_palette[[4]]}'>{names(col_palette)[[4]]}</span>, and 
  <span style='color:{col_palette[[5]]}'>{names(col_palette)[[5]]}</span> countries."
)
cap <- paste0(
  st,
  "<br><br>**Data**: Wikipedia<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

# Spiral plot
png(
  filename = file.path("2024", "2024-08-13", paste0("20240813_spiral", ".png")),
  bg = "transparent",
  height = 3.5, width = 3.5, units = "in", res = 300
)
spiral_initialize(xlim = c(as.numeric(as.Date("1850-01-01")), as.numeric(as.Date("2021-12-31"))), polar_lines_gp = gpar(col = "white", lty = 3))
spiral_track(ylim = c(0, 1), background_gp = gpar(col = NA, fill = "grey90"))
spiral_rect(
  xleft = plot_data$start_date_num,
  xright = plot_data$end_date_num,
  ybottom = 0,
  ytop = 1,
  gp = gpar(fill = plot_data$value, col = plot_data$value)
)
dev.off()

# Combine
ggplot() +
  geom_textbox(
    mapping = aes(x = 6.25, y = 3, label = title),
    family = title_font,
    fontface = "bold",
    colour = text_col,
    box.colour = NA,
    fill = NA,
    hjust = 0.5,
    halign = 0.5,
    lineheight = 0.5,
    size = 18,
    width = unit(4, "inch")
  ) +
  geom_textbox(
    mapping = aes(x = 6.25, y = 1.5, label = cap),
    family = body_font,
    colour = text_col,
    box.colour = NA,
    fill = NA,
    hjust = 0.5,
    halign = 0.5,
    lineheight = 0.5,
    size = 9,
    width = unit(3.5, "inch")
  ) +
  geom_image(
    data = slice_head(plot_data, n = 1),
    aes(
      x = 2,
      y = 2,
      image = "2024/2024-08-13/20240813_spiral.png"
    ),
    size = 1
  ) +
  annotate("text",
    x = 3.84, y = 2.15, label = "2022",
    family = body_font,
    colour = text_col,
    size = 9
  ) +
  annotate("text",
    x = 2.47, y = 1.9, label = "1850",
    family = body_font,
    colour = text_col,
    size = 9
  ) +
  theme_void(base_size = 20, base_family = body_font) +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 3.75)) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-08-13", paste0("20240813", ".png")),
  width = 8,
  height = 4
)

gg_playback(
  name = file.path("2024", "2024-08-13", paste0("20240813", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
