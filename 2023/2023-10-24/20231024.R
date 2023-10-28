# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(emojifont)
library(clustMixType)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-24")
patient_risk_profiles <- tuesdata$patient_risk_profiles


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey98"
text_col <- "grey30"
text_col_dark <- "grey10"
col_palette <- PrettyCols::prettycols("Fun")[c(1,2,3,5)]
highlight_col <- col_palette[4]

body_font <- "roboto"
title_font <- "robotoslab"

# Data wrangling ----------------------------------------------------------

# cluster data
plot_data <- patient_risk_profiles 
cluster_data <- patient_risk_profiles |> 
  select(-personId) |> 
  mutate(across(`age group:  10 -  14`:`Antibiotics Tetracyclines in prior year`, 
                factor))

# plot elbow plot / silhouette
Kwss <- numeric(10)
for(i in 1:10){
  kpres <- kproto(cluster_data, k = i, nstart = 5)
  Kwss[i] <- kpres$tot.withinss
}
plot(1:10, Kwss, type = "b",
     ylab = "Objective Function", xlab = "# Clusters",
     main = "Scree Plot")
# choose k = 4

# final clustering
kpres <- kproto(cluster_data, k = 4, nstart = 5)
patient_risk_profiles$cluster <- kpres$cluster

# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-10-24", "recording"),
  device = "png",
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300
)


# Plot --------------------------------------------------------------------

# plot clustering
cluster_plot_data <- patient_risk_profiles |> 
  select(cluster, `predicted risk of Pulmonary Embolism`, starts_with("age")) |> 
  pivot_longer(starts_with("age"),
               names_to = "age_group",
               values_to = "binary") |> 
  filter(binary == 1) |> 
  mutate(`predicted risk of Pulmonary Embolism` = 100 * `predicted risk of Pulmonary Embolism`) |> 
  mutate(age_group = stringr::str_remove_all(
    stringr::str_remove(age_group, "age group:"), " ")) |> 
  mutate(age_group = factor(age_group),
         age_group = fct_relevel(age_group, "5-9", after = 0),
         age_group = fct_relevel(age_group, "0-4", after = 0)) |> 
  select(-binary)

set.seed(123)
ggplot(data = cluster_plot_data) +
  geom_jitter(mapping = aes(
    x = age_group,
    y = `predicted risk of Pulmonary Embolism`,
    colour = factor(cluster)
  ),
  width = 0.4,
  height = 0,
  size = 3,
  alpha = 0.7) +
  scale_colour_manual(
    values = col_palette
  ) +
  labs(y = "Predicted risk of pulmonary embolism (%)",
       x = "Age group",
       title = "Clustering of patients",
       caption = social,
       subtitle = "100 simulated patient's medical history features and their predicted 1-year risk of 14 outcomes are clustered using k-prototypes clusering.") +
  theme_bw(base_size = 35) +
  theme(
    legend.position = c(0.05, 0.9),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    plot.margin = margin(5, 5, 5, 5),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      face = "bold",
      lineheight = 0.5,
      margin = margin(l = 10, t = 5, b = 0),
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      lineheight = 0.5,
      margin = margin(l = 10, t = 10, b = 5),
      family = body_font
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      lineheight = 0.5,
      margin = margin(l = 10, t = 10, b = 5),
      family = body_font
    )
  )

# average patient profile
mode_age <- function(data) {
  data |>
    select(starts_with("age")) |>
    colSums() |>
    sort(decreasing = TRUE) |>
    _[1] |>
    names() |>
    stringr::str_remove("age group:") |>
    stringr::str_remove_all(" ")
}

mode_sex <- function(data) {
  data |>
    select(starts_with("sex")) |>
    colSums() |>
    sort(decreasing = TRUE) |>
    _[1] |>
    names() |>
    stringr::str_remove("Sex =") |>
    stringr::str_remove_all(" ") |> 
    stringr::str_to_sentence()
}

COPD <- function(data) {
  value <- table(
    data[["Chronic obstructive pulmonary disease (COPD) in prior year"]]
  ) |> 
    sort(decreasing = TRUE) |>
    _[1] |> 
    names() 
  text <- ifelse(value == 1, "Yes", "No")
  avg_perc <- sum(patient_risk_profiles$`Chronic obstructive pulmonary disease (COPD) in prior year`)/nrow(patient_risk_profiles)
  data_perc <- sum(data$`Chronic obstructive pulmonary disease (COPD) in prior year`)/nrow(data)
  if (data_perc > avg_perc) {
    glue::glue(
      "<span style='color:#df2935;'>{text}</span>"
    )
  } else {
    text
  }
}

HF <- function(data) {
  value <- table(
    data[["Heart failure in prior year"]]
  ) |> 
    sort(decreasing = TRUE) |>
    _[1] |> 
    names() 
  text <- ifelse(value == 1, "Yes", "No")
  avg_perc <- sum(patient_risk_profiles$`Heart failure in prior year`)/nrow(patient_risk_profiles)
  data_perc <- sum(data$`Heart failure in prior year`)/nrow(data)
  
  if (data_perc > avg_perc) {
    glue::glue(
      "<span style='color:#df2935;'>{text}</span>"
    )
  } else {
    text
  }
}

PE_risk <- function(data) {
  value <- 100 * mean(
    data[["predicted risk of Pulmonary Embolism"]],
    na.rm = TRUE
  )
  avg_value <- 100 * mean(
    patient_risk_profiles[["predicted risk of Pulmonary Embolism"]],
    na.rm = TRUE
  )
  if (value > avg_value) {
    text <- paste0(round(value, 2), "%")
    glue::glue(
      "<span style='color:#df2935;'>{text}</span>"
    )
  } else {
    paste0(round(value, 2), "%")
  }
}

geom_category <- function(y, label) {
  geom_richtext(
    data = data.frame(),
    mapping = aes(
      x = -1, y = y,
      label = label
    ),
    family = body_font,
    label.colour = "transparent",
    fill = "transparent",
    size = 12,
    hjust = 0,
    colour = text_col
  )
}

# patient profile plot function
profile_plot <- function(
    plot_data, profile_label = "All patients", colour = "blue"
) {
  ggplot() +
    geom_rect(
      data = data.frame(),
      mapping = aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1),
      fill = colour,
      alpha = 0.5
    ) +
    geom_text(
      data = data.frame(),
      mapping = aes(x = 0, y = 0, label = fontawesome("fa-user")),
      family = "fontawesome-webfont",
      size = 180,
      colour = colour
    ) +
    geom_category(
      y = -1.3,
      label = glue::glue(
        "**Patient group** : {profile_label}"
      )
    ) +
    geom_category(
      y = -1.6,
      label = glue::glue(
        "**Patient group size** : {nrow(plot_data)}"
      )
    ) +
    geom_category(
      y = -1.9,
      label = glue::glue(
        "**Age group** : {mode_age(plot_data)}"
      )
    ) +
    geom_category(
      y = -2.2,
      label = glue::glue(
        "**Sex** : {mode_sex(plot_data)}"
      )
    ) +
    geom_category(
      y = -2.5,
      label = glue::glue(
        "**COPD (previous year)** : {COPD(plot_data)}"
      )
    ) +
    geom_category(
      y = -2.8,
      label = glue::glue(
        "**Heart failure (previous year)** : {HF(plot_data)}"
      )
    ) +
    geom_category(
      y = -3.1,
      label = glue::glue(
        "**Avg. pulmonary embolism risk** : {PE_risk(plot_data)}"
      )
    ) +
    scale_y_continuous(limits = c(-3.1, 1)) +
    labs(
      title = "Patient Risk Profile",
      subtitle = "Average patient group statistics"
    ) +
    theme_void(base_size = 22) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 5),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      plot.background = element_rect(fill = bg_col, colour = text_col),
      plot.title = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        face = "bold",
        size = 42,
        lineheight = 0.5,
        margin = margin(l = 10, t = 5, b = 0),
        family = title_font
      ),
      plot.subtitle = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        size = 32,
        lineheight = 0.5,
        margin = margin(l = 10, t = 10, b = -5),
        family = body_font
      )
    )
}

p1 <- profile_plot(filter(patient_risk_profiles, cluster == 1),
             profile_label = "Cluster 1",
             colour = col_palette[1])
p2 <- profile_plot(filter(patient_risk_profiles, cluster == 2),
             profile_label = "Cluster 2",
             colour = col_palette[2])
p3 <- profile_plot(filter(patient_risk_profiles, cluster == 3),
             profile_label = "Cluster 3",
             colour = col_palette[3])
p4 <- profile_plot(filter(patient_risk_profiles, cluster == 4),
             profile_label = "Cluster 4",
             colour = col_palette[4])

# join together to 4 
p1 + p2 + p3 + p4 + 
  plot_layout(ncol = 4) +
  plot_annotation(caption = paste0("\\* <span style='color:#df2935;'>red text</span> indicates above average percentage<br>", social)) &
  theme(
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      size = 24,
      halign = 0,
      lineheight = 0.5,
      margin = margin(l = 0, t = 10, b = 5),
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-10-24", paste0("20231024", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
