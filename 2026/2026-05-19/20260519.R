# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(rvest)
library(ggh4x)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-05-19")
member_participation_stats_by_country <- tuesdata$member_participation_stats_by_country
metadata_coverage_stats_by_country <- tuesdata$metadata_coverage_stats_by_country

url <- "https://esgdata.worldbank.org/about/faq?lang=en"
raw_html <- read_html(url)
wb_codes <- raw_html |> 
  html_elements(".datatable") |> 
  html_table() |> 
  _[[2]]

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
highlight_col <- "#A6CE39"


# Data wrangling ----------------------------------------------------------

plot_data <- member_participation_stats_by_country |>
  select(current_up_to, region_id, total_members, deposits_orcid) |>
  group_by(current_up_to, region_id) |>
  summarise(
    total_members = sum(total_members),
    deposits_orcid = sum(deposits_orcid)
  ) |>
  mutate(perc = round(100 * deposits_orcid / total_members),
         perc_label = paste0(" ", perc, "%")) |>
  ungroup() |> 
  left_join(wb_codes, by = c("region_id" = "Abbreviation"))

last_data <- plot_data |> 
  slice_max(current_up_to) |> 
  arrange(desc(perc)) |> 
  mutate(Description = factor(Description, levels = Description))

plot_data$Description <- factor(plot_data$Description, levels = last_data$Description)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- glue("<span style='font-family:{title_font}; font-size: 15pt;'>**Crossref members increasing across the globe**</span>")
st <- paste0(title, "<br><br>In addition to providing Digital Object Identifiers (DOIs), Crossref maintain a repository of metadata which makes research discoverable, linkable, and reusable. Usage of Crossref varies widely across the globe, with relatively few members in Sub-Saharan Africa. The total area in each chart shows the number of members in each region with at least one registered DOI, with the <span style='color:#83A329;'>**green area showing the number who have deposited at least one work with an ORCID ID**</span>.")
cap <- source_caption(source = "Crossref. World Bank.", graphic = social)


# Plot --------------------------------------------------------------------

design <- "
  ##A
  BCD
  EFH
"

ggplot() +
  geom_area(
    data = plot_data,
    mapping = aes(x = current_up_to, y = total_members),
    fill = "grey65",
    alpha = 0.8
  ) +
  geom_area(
    data = plot_data,
    mapping = aes(x = current_up_to, y = deposits_orcid),
    fill = highlight_col
  ) +
  geom_line(
    data = plot_data,
    mapping = aes(x = current_up_to, y = deposits_orcid),
    colour = "#83A329"
  ) +
  geom_point(
    data = last_data,
    mapping = aes(x = current_up_to, y = deposits_orcid),
    colour = "#83A329"
  ) +
  geom_text(
    data = last_data,
    mapping = aes(x = current_up_to, y = deposits_orcid, label = perc_label),
    colour = "#83A329",
    family = body_font,
    hjust = 0,
    fontface = "bold",
    size.unit = "pt",
    size = 12
  ) +
  labs(x = NULL, y = NULL,
       tag = st,
       caption = cap) +
  facet_manual(vars(Description), design = design, axes = "x") +
  scale_y_continuous(labels = scales::label_comma()) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 11, base_family = body_font) +
  theme(
    plot.margin = margin(5, 40, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font,
      maxwidth = 0.63,
      vjust = 1,
      valign = 1,
      size = rel(0.95)
    ),
    plot.tag.position = c(0.01, 0.99),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 20, l = 5),
      size = rel(0.9),
      height = unit(2, "lines"),
      width = 1.1,
      vjust = 1,
      valign = 1,
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.x = unit(2.5, "lines"),
    panel.spacing.y = unit(1, "lines")
  ) +
  canvas(
    width = 7, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-05-19", paste0("20260519", ".png"))
)
