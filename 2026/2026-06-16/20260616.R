## Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


## Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-06-16")
england_wales_names <- tuesdata$england_wales_names |>
  mutate(Region = "England & Wales")
ni_names <- tuesdata$ni_names |>
  mutate(Region = "Northern Ireland")
scotland_names <- tuesdata$scotland_names |>
  mutate(Region = "Scotland")
all_names <- rbind(rbind(ni_names, scotland_names), england_wales_names)


## Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


## Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"
comp_col <- "#197176"


## Data wrangling ----------------------------------------------------------

girl_names <- all_names |>
  dplyr::filter(Sex == "Girl") |>
  tidyr::drop_na()
boy_names <- all_names |>
  dplyr::filter(Sex == "Boy") |>
  tidyr::drop_na()

min_year <- girl_names |>
  group_by(Region) |>
  slice_min(Year, with_ties = FALSE) |>
  ungroup() |>
  slice_max(Year) |>
  pull(Year)
max_year <- girl_names |>
  group_by(Region) |>
  slice_max(Year, with_ties = FALSE) |>
  ungroup() |>
  slice_min(Year) |>
  pull(Year)
chosen_names <- c("George", "Charlotte", "Louis")

plot_data <- rbind(girl_names, boy_names) |>
  dplyr::filter(
    Name %in% chosen_names,
    Year >= min_year,
    Year <= max_year
  ) |>
  dplyr::filter_out(Name == "George" & Sex == "Girl")

annotate_dates <- data.frame(
  Name = chosen_names,
  Year = c(2013, 2015, 2018)
)

plot_data$Name <- factor(plot_data$Name, levels = chosen_names)
annotate_dates$Name <- factor(annotate_dates$Name, levels = chosen_names)


## Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Spike in popularity of names after royal births"
st <- glue("Trends in baby names can often be linked to popular culture, with names chosen from films and current events. When a new royal baby is born, a spike in popularity of the name given often happens in the following year. Particularly for the name Charlotte, which saw an increase in popularity in all three regions.<br><br><span style='font-size:9pt;'>Number of babies born. 1997 to 2024.</span>")
cap <- paste0("**Note**: Black vertical lines indicate year of birth for Prince George, Princess Charlotte, and Prince Louis.<br>", source_caption(source = "Office for National Statistics | Northern Ireland Statistics and Research Agency | National Records Scotland", graphic = social))


## Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = Year, y = Number, fill = Sex, colour = Sex)
) +
  geom_area(alpha = 0.3) +
  geom_line() +
  geom_vline(
    data = annotate_dates,
    mapping = aes(xintercept = Year)
  ) +
  facet_wrap(~Name+Region, scales = "free_y", ncol = 3, axes = "all_x",
             labeller = \(x) label_value(x, multi_line = FALSE)) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE),
                     expand = expansion(0, 0)) +
  scale_y_continuous(labels = scales::label_comma(),
                     expand = expansion(c(0, 0.1), 0)) +
  scale_fill_manual(values = c("Girl" = highlight_col, "Boy" = comp_col)) +
  scale_colour_manual(values = c("Girl" = highlight_col, "Boy" = comp_col)) +
  labs(
    x = NULL, y = NULL,
    title = title,
    subtitle = st,
    caption = cap) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = body_font, base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
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
      margin = margin(b = 10, t = 0),
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
      margin = margin(t = 0, b = 5),
      family = body_font,
      hjust = 0,
      lineheight = 0.4,
      size = rel(0.9)
    ),
    axis.title.x = element_text(
      hjust = 1, colour = alpha(text_col, 0.7),
      margin = margin(r = -5, t = 3)
    ),
    panel.spacing = unit(1, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.4, colour = "grey50"),
    plot.margin = margin(5, 10, 5, 5)
  ) +
  canvas(
    width = 7, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


## Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-06-16", paste0("20260616", ".png"))
)




# Plot 2 ------------------------------------------------------------------

## Data wrangling ----------------------------------------------------------

plot_data <- all_names |> 
  select(-Rank) |> 
  drop_na(Number) |> 
  filter(Number >= 3) |> 
  group_by(Name, Year, Sex) |> 
  mutate(Number = sum(Number)) |> 
  ungroup() |> 
  select(-Region) |> 
  distinct() |> 
  pivot_wider(names_from = Sex, values_from = Number) |> 
  drop_na(Boy, Girl) |> 
  filter(Year >= 1997, Year <= 2024) |> 
  group_by(Name) |> 
  mutate(n_years = n()) |> 
  ungroup() |> 
  filter(n_years == 28) |> 
  select(-n_years) |> 
  mutate(Total = Boy + Girl,
         Girl_p = Girl / Total,
         Boy_p = Boy / Total) |> 
  arrange(abs(0.5 - Girl_p)) |> 
  group_by(Name) |> 
  mutate(Total_all = sum(Total)) |> 
  ungroup() |> 
  group_by(Name) |> 
  arrange(-Year) |> 
  mutate(min_perc_g = Girl_p[1]) |> 
  mutate(min_perc_b = Boy_p[1]) |> 
  ungroup() |> 
  filter(min_perc_g >= 0.3, min_perc_g <= 0.7) |> 
  filter(Total_all >= 4000) |> 
  select(-ends_with("_p")) |> 
  select(-Total_all) |> 
  select(-starts_with("min_perc")) |> 
  pivot_longer(cols = c(Boy, Girl),
               values_to = "Number",
               names_to = "Sex")

labels <- plot_data |> 
  filter(Year == 2024, Sex == "Girl") |> 
  mutate(perc = round(100 * Number / Total)) |> 
  arrange(desc(perc))

labels$Name <- factor(labels$Name, levels = labels$Name)
plot_data$Name <- factor(plot_data$Name, levels = labels$Name)


## Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Most popular gender neutral names"
st <- glue("Percentage of babies born that are <span style='color:{highlight_col};'>**girls**</span> and <span style='color:{comp_col};'>**boys**</span>. 1997 to 2024.")
cap <- paste0("**Note**: Includes names where percentage of names given to baby girls is between 30% and 70% in 2024, with at least 4,000 babies named between 1997 and 2024 across the UK.<br>", source_caption(source = "Office for National Statistics | Northern Ireland Statistics and Research Agency | National Records Scotland", graphic = social))


## Plot --------------------------------------------------------------------

ggplot() +
  annotate("segment", x = 1997, xend = 2024, y = 0.5, yend = 0.5,
           colour = text_col, alpha = 0.6) +
  geom_area(
    data = plot_data,
    mapping = aes(x = Year, y = Number, fill = Sex),
    position = "fill",
    alpha = 0.5
  ) +
  geom_line(
    data = plot_data,
    mapping = aes(x = Year, y = Number, colour = Sex),
    position = "fill"
  ) +
  geom_point(
    data = labels,
    mapping = aes(x = 2024, y = perc / 100),
    colour = highlight_col
  ) +
  geom_text(
    data = labels,
    mapping = aes(x = 2024.5, y = perc / 100, label = paste0(" ", perc, "%")),
    hjust = 0,
    size = 3.4,
    colour = highlight_col,
    fontface = "bold"
  ) +
  annotate("text", x = 2023.25, y = 0.05, label = "Girls",
           colour = highlight_col, fontface = "bold",
           hjust = 1) +
  annotate("text", x = 2023.25, y = 0.95, label = "Boys",
           colour = comp_col, fontface = "bold",
           hjust = 1, vjust = 0.5) +
  facet_wrap(~Name, ncol = 4, axes = "all_x") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = c("Girl" = highlight_col, "Boy" = comp_col)) +
  scale_colour_manual(values = c("Girl" = highlight_col, "Boy" = "transparent")) +
  labs(
    x = NULL, y = NULL,
    title = title,
    subtitle = st,
    caption = cap) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_family = body_font, base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
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
      family = body_font,
      width = 1.063
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 0, b = 5),
      family = title_font,
      hjust = 0,
      lineheight = 0.4,
      size = rel(1)
    ),
    axis.title.x = element_text(
      hjust = 1, colour = alpha(text_col, 0.7),
      margin = margin(r = -5, t = 3)
    ),
    panel.spacing.y = unit(1, "lines"),
    panel.spacing.x = unit(2, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.4, colour = "grey50"),
    plot.margin = margin(10, 35, 10, 10)
  ) +
  canvas(
    width = 7, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


## Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-06-16", paste0("20260616_v2", ".png"))
)


