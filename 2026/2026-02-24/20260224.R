# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-02-24")
sfi_grants <- tuesdata$sfi_grants


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

plot_data <- sfi_grants |>
  drop_na(proposal_title) |> 
  mutate(
    start_year = year(start_date),
    science = str_detect(str_to_lower(proposal_title), "science"),
    technology = str_detect(str_to_lower(proposal_title), "technology"),
    engineering = str_detect(str_to_lower(proposal_title), "engineering"),
    mathematics = str_detect(str_to_lower(proposal_title), "mathematics|mathematical")
  ) |>
  select(start_year, science:mathematics) |>
  pivot_longer(-start_year, names_to = "subject") |>
  group_by(start_year, subject) |>
  summarise(n = sum(value)) |>
  ungroup() |> 
  filter(start_year < 2025)

col_palette <- c('#4477AA', '#EE6677', '#228833', '#C0AE35')
names(col_palette) <- unique(plot_data$subject)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = col_palette[1],
  font_colour = text_col,
  font_family = body_font
)
title <- "Increase in number of 'science' grants after launch of Discover Programme"
st <- glue("Science Foundation Ireland (SFI) was the national foundation in Ireland for investment in scientific and engineering research. SFI invested in academic researchers and research teams who were most likely to generate new knowledge, leading edge technologies and competitive enterprises in the fields of <span style='color:{col_palette[[\"science\"]]};'>**science**</span>, <span style='color:{col_palette[[\"technology\"]]};'>**technology**</span>, <span style='color:{col_palette[[\"engineering\"]]};'>**engineering**</span> and <span style='color:{col_palette[[\"mathematics\"]]};'>**mathematics**</span> (STEM).")
cap <- paste0("**Note**: Number of grants determined based on whether the subject name is included in the proposal title, with *mathematics* extended to include the word *mathematical*. Awards may be counted multiple times.<br>", source_caption(source = "Ireland's Open Data Portal", graphic = social, sep = " | "))


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = start_year, y = n, colour = subject)
) +
  geom_line(linewidth = 1) +
  geom_point(
    data = filter(plot_data, start_year == 2024),
    size = 1.5
  ) +
  geom_text(
    data = plot_data |> 
      filter(start_year == 2024) |> 
      mutate(n = case_when(subject == "mathematics" ~ n - 1, 
                           subject == "engineering" ~ n + 1,
                           TRUE ~ n)),
    mapping = aes(x = start_year + 0.25, label = str_to_title(subject)),
    hjust = 0,
    fontface = "bold"
  ) +
  geom_textbox(
    data = data.frame(x = 2013, y = 47, label = "**SFI Discover Programme Launches**<br>Applicants are encouraged to submit proposals that create opportunities for broader participation and engagement of the public with STEM."),
    mapping = aes(x = x, y = y, label = label),
    fill = "transparent",
    box.colour = "transparent",
    family = body_font,
    inherit.aes = FALSE,
    hjust = 1, halign = 1,
    valign = 1, vjust = 1,
    size = 3.5,
    minwidth = unit(2.8, "in")
  ) +
  geom_vline(xintercept = 2013, linetype = "dashed",
             colour = text_col) +
  labs(x = NULL, y = "Number of grants starting per year", title = title,
       subtitle = st, caption = cap) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_colour_manual(values = col_palette) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 70, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0),
      family = title_font,
      face = "bold",
      size = rel(1.35)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 30, t = 5),
      family = body_font,
      width = 1.1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font,
      width = 1.1
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank(),
    axis.title.y.left = element_text(angle = 0, 
                                     vjust = 1.08,
                                     margin = margin(r = -157),
                                     colour = alpha(text_col, 0.6))
  ) +
  canvas(
    width = 7, height = 5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-02-24", paste0("20260224", ".png"))
)
