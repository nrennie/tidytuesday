# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(marquee)
library(waffle)
library(ragg)


# Load data ---------------------------------------------------------------

wind_turbines <- openxlsx::read.xlsx("https://ftp.cartes.canada.ca/pub/nrcan_rncan/Wind-energy_Energie-eolienne/wind_turbines_database/Wind_Turbine_Database_FGP.xlsx", sheet = 1)
write.csv(wind_turbines, "2020/2020-10-27/data/wind_turbines.csv", row.names = FALSE)
wind_turbines <- readr::read_csv("2020/2020-10-27/wind_turbines.csv")


# Load fonts --------------------------------------------------------------

sysfonts::font_add(
  family = "Font Awesome 6",
  regular = "fonts/Font Awesome 6 Free-Solid-900.otf"
)
showtext_auto()


# Data wrangling ----------------------------------------------------------

extract_after_last_slash <- function(texts) {
  has_slash <- stringr::str_detect(texts, "/")
  extracted <- stringr::str_match(texts, ".*/(.*)$")[, 2]
  output <- dplyr::if_else(has_slash, extracted, texts)
  return(output)
}

turbines_year <- wind_turbines |>
  dplyr::select(`Province/Territory`, Commissioning.date) |>
  dplyr::mutate(
    Year = extract_after_last_slash(Commissioning.date),
    Year = as.numeric(Year)
  )

## multiple "other"
plot_data <- turbines_year |>
  dplyr::filter(Year >= 2001 & Year <= 2020) |>
  dplyr::mutate(
    Year_Group = dplyr::case_when(
      Year %in% seq(2001, 2005) ~ "2001 - 2005",
      Year %in% seq(2006, 2010) ~ "2006 - 2010",
      Year %in% seq(2011, 2015) ~ "2011 - 2015",
      Year %in% seq(2016, 2020) ~ "2016 - 2020"
    )
  ) |>
  dplyr::mutate(
    Year_Group = factor(Year_Group, levels = c(
      "2001 - 2005", "2006 - 2010", "2011 - 2015", "2016 - 2020"
    ))
  ) |>
  dplyr::rename(Area = `Province/Territory`) |>
  dplyr::mutate(
    Area = case_when(
      Area %in% c(
        "Northwest Territories",
        "Newfoundland and Labrador",
        "Prince Edward Island",
        "New Brunswick",
        "Manitoba",
        "Saskatchewan"
      ) ~ "other",
      TRUE ~ Area
    )
  ) |>
  dplyr::count(Area, Year_Group) |>
  dplyr::mutate(n = round(n / 20)) |>
  dplyr::filter(n != 0)

area_levels <- c(plot_data |> 
  summarise(n = sum(n), .by = Area) |> 
  arrange(-n) |> 
  filter(Area != "other") |> 
  pull(Area), "other")
plot_data$Area <- factor(plot_data$Area, levels = area_levels)


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
col_palette <- rcartocolor::carto_pal(length(unique(plot_data$Area)) + 1, "Vivid")[1:length(unique(plot_data$Area))]
names(col_palette) <- levels(plot_data$Area)
highlight_col <- col_palette[1]


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2020", "2020-10-27", "recording"),
  device = agg_png,
  width = 6,
  height = 4.5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "sans"
)
title <- "**Canadian wind turbines are mostly found in Ontario**"
st <- glue(
  "The Canadian Wind Turbine Database contains the geographic location
  and key technology details for wind turbines installed in Canada. It includes
  information about turbines installed in {.[col_palette[[1]]] [names(col_palette)[[1]]]}, 
  {.[col_palette[[2]]] [names(col_palette)[[2]]]}, {.[col_palette[[3]]] [names(col_palette)[[3]]]}, 
  {.[col_palette[[4]]] [names(col_palette)[[4]]]}, {.[col_palette[[5]]] [names(col_palette)[[5]]]}, 
  and {.[col_palette[[6]]] [names(col_palette)[[6]]]} regions.",
  .open = "[",
  .close = "]"
)
cap <- paste0(
  "**Data**: Natural Resources Canada<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_pictogram(
    mapping = aes(
      label = Area,
      colour = Area,
      values = n
    ),
    flip = TRUE,
    n_rows = 10,
    size = 7,
    family = "Font Awesome 6"
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("fan", "fan", "fan", "fan")
  ) +
  facet_wrap(~Year_Group, nrow = 1, strip.position = "bottom") +
  scale_color_manual(
    values = col_palette
  ) +
  scale_x_discrete(
    expand = c(0, 0, 0, 0)
  ) +
  scale_y_continuous(
    labels = function(x) format(x * 10 * 20, big.mark = ","),
    expand = c(0, 0),
    breaks = c(0, 5, 10, 15, 20),
    limits = c(0, 20),
    minor_breaks = NULL
  ) +
  coord_fixed() +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_minimal(
    base_size = 28
  ) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(5, 15, 5, 15),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.grid.major = element_line(
      linewidth = 0.4
    ),
    plot.title = element_marquee(
      colour = text_col,
      hjust = 0,
      lineheight = 0.5,
      width = 1,
      size = 14,
      margin = margin(b = -10)
    ),
    plot.subtitle = element_marquee(
      colour = text_col,
      hjust = 0,
      width = 1,
      size = 10
    ),
    plot.caption = element_textbox_simple(
      hjust = 0,
      halign = 0,
      lineheight = 0.5,
      size = 20
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2020", "2020-10-27", paste0("20201027", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
