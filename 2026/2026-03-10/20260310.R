# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-03-10")
absolute_judgements <- tuesdata$absolute_judgements
pairwise_comparisons <- tuesdata$pairwise_comparisons
respondent_metadata <- tuesdata$respondent_metadata


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
font_add("FA",
         regular = "fonts/Font Awesome 6 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#797FCD"


# Data wrangling ----------------------------------------------------------

term_ranks <- absolute_judgements |>
  left_join(respondent_metadata, by = "response_id") |>
  filter(country_of_residence %in% c("United States", "United Kingdom")) |>
  drop_na(country_of_residence) |> 
  group_by(term) |>
  summarise(
    med_prob = median(probability)
  ) |>
  arrange(desc(med_prob))

plot_data <- absolute_judgements |>
  mutate(term = factor(term, levels = term_ranks$term)) |>
  left_join(respondent_metadata, by = "response_id") |>
  filter(country_of_residence %in% c("United States", "United Kingdom")) |>
  drop_na(country_of_residence)

summary_data <- plot_data |>
  group_by(country_of_residence, term) |>
  summarise(med_prob = median(probability)) |>
  ungroup()


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Do Americans and Brits agree about what is 'likely'?"
icon_us <- "<span style='font-size:8pt;font-family:\"FA\";color:#5B62C2;'>&#xf111;</span> "
icon_uk <- "<span style='font-size:8pt;font-family:\"FA\";color:#F97639;'>&#xf219;</span> "
st <- glue("In an online quiz, {format(nrow(respondent_metadata), big.mark = ',')} participants assigned numerical values (0–100%) to each of 19 probabilistic phrases. Across {format(length(unique(plot_data$response_id)), big.mark = ',')} participants from the <span style='color:#5B62C2;'>**United States**</span> {icon_us} or the <span style='color:#F97639;'>**United Kingdom**</span> {icon_uk}, responses generally did not vary by country of residence. However, for the terms *highly likely*, *may happen*, and *might happen*, the responses differed. Those from the United States rated all three as being more likely than British people on average.")
cap <- paste0("**Note**: Responses from participants outside of the United States and United Kingdom, and from those who did not provide their country of residence, are excluded. Terms are ranked by overall median probability.<br>", source_caption(source = "Kucharski AJ (2026) CAPphrase: Comparative and Absolute Probability phrase dataset. DOI: 10.5281/zenodo.18750055", graphic = social))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_rect(
    data = filter(plot_data, country_of_residence == "United States"),
    mapping = aes(
      xmin = probability - 0.5,
      xmax = probability + 0.5,
      ymin = 0, ymax = 1,
      fill = country_of_residence
    ),
    colour = "transparent",
    alpha = 0.1
  ) +
  geom_rect(
    data = filter(plot_data, country_of_residence == "United Kingdom"),
    mapping = aes(
      xmin = probability - 0.5,
      xmax = probability + 0.5,
      ymin = -1, ymax = 0,
      fill = country_of_residence
    ),
    colour = "transparent",
    alpha = 0.1
  ) +
  geom_point(
    data = summary_data,
    mapping = aes(
      x = med_prob,
      y = if_else(country_of_residence == "United States", 0.5, -0.5),
      fill = country_of_residence,
      shape = country_of_residence
    ),
    colour = text_col,
    size = 2.5,
    alpha = 1
  ) +
  facet_wrap(~term, ncol = 1, strip.position = "left") +
  labs(
    x = "Probability (%)",
    y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_x_continuous(expand = expansion(0, 0)) +
  scale_shape_manual(values = c(23, 21)) +
  scale_fill_manual(
    values = c(
      "United States" = highlight_col,
      "United Kingdom" = "#FA9161"
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
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
    strip.text.y.left = element_text(
      face = "bold",
      angle = 0,
      hjust = 1,
      size = rel(0.9)
    ),
    panel.spacing = unit(0.25, "lines"),
    axis.title.x = element_text(hjust = 1, margin = margin(r = -5)),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  canvas(
    width = 5, height = 7.5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-03-10", paste0("20260310", ".png"))
)
