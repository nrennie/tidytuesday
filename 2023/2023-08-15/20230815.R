date_chr <- "2023-08-15"
date_strip <- stringr::str_remove_all(date_chr, "-")

# Inspired by: https://github.com/doehm/tidytues/blob/main/scripts/2023/week-29-gpt/gpt.R

# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggfx)

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(date_chr)
spam <- tuesdata$spam

# Load fonts --------------------------------------------------------------

font_add_google("Space Mono", "SpaceMono")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "#CECECE"
text_col <- "#06070E"
highlight_col <- "#AD343E"


# Data wrangling ----------------------------------------------------------

spam_data <- spam |>
  select(money, n000, make, yesno) |>
  mutate(yesno = case_when(
    yesno == "y" ~ "Spam",
    yesno == "n" ~ "Not Spam"
  )) |>
  mutate(across(money:make, ~ .x > 0)) |>
  group_by(yesno) |>
  mutate(n = n()) |>
  pivot_longer(money:make, names_to = "type") |>
  group_by(yesno, type) |>
  mutate(n_type = sum(value)) |>
  select(-value) |>
  distinct() |>
  mutate(perc = n_type / n) |>
  select(yesno, type, perc)

plot_data <- spam_data |>
  ungroup() |>
  mutate(
    y = as.numeric(factor(type,
                          levels = c("money", "make", "n000"))),
    xmin = ifelse(yesno == "Spam", -0.05 - sqrt(perc), 0.05),
    xmax = ifelse(yesno == "Spam", -0.05, 0.05 + sqrt(perc)),
    ymax = y + sqrt(perc),
    type = case_when(type == "n000" ~ "000",
                     TRUE ~ type)
  ) 

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", date_chr, "recording"),
  device = "png",
  width = 4,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "SpaceMono"
)
title <- "Spam Emails"
st <- glue::glue("Of 4601 e-mails classified as either as 
                 <span style='color:{highlight_col}'>**spam**</span> or **non-spam**, those classified 
                 as spam were much more likely to contain the words *make*, *money*, or a string of 
                 three zeros, *000*.
                 <br>**Data**: {{Rdatasets}}")


# Plot --------------------------------------------------------------------

ggplot(plot_data) +
  # main squares
  geom_rect(
    data = plot_data,
    mapping = aes(xmin = xmin, xmax = xmax,
                  ymin = y, ymax = ymax,
                  fill = yesno)
  ) +
  # faded inner squares
  with_inner_glow(
    geom_rect(
      data = filter(plot_data, yesno == "Spam"),
      mapping = aes(
        xmin = xmin + 0.03, xmax = xmax - 0.03,
        ymin = y + 0.03, ymax = ymax - 0.03
      ),
      fill = bg_col
    ),
    colour = highlight_col, sigma = 30
  ) +
  with_inner_glow(
    geom_rect(
      data = filter(plot_data, yesno == "Not Spam"),
      mapping = aes(
        xmin = xmin + 0.03, xmax = xmax - 0.03,
        ymin = y + 0.03, ymax = ymax - 0.03
      ),
      fill = bg_col
    ),
    colour = text_col, sigma = 30
  ) +
  # add text labels
  geom_text(
    data = plot_data,
    mapping = aes(
      x = xmin + 0.5 * (xmax - xmin),
      y = y + 0.5 * (ymax - y),
      label = paste0(round(100 * perc), "%"),
      colour = perc < 0.05,
    ),
    hjust = 0.5,
    size = 11,
    family = "SpaceMono"
  ) +
  geom_segment(
    data = filter(plot_data, yesno == "Not Spam"),
    mapping = aes(x = 0.05, xend = 0.7, y = y - 0.05, yend = y - 0.05),
    linewidth = 0.5,
    colour = highlight_col
  ) +
  geom_text(
    data = filter(plot_data, yesno == "Not Spam"),
    mapping = aes(x = 0.7, y = y, label = type),
    hjust = 1,
    colour = highlight_col,
    size = 9,
    family = "SpaceMono"
  ) +
  # colours
  scale_fill_manual(values = c("Spam" = highlight_col, "Not Spam" = text_col)) +
  scale_colour_manual(values = c(text_col, bg_col)) +
  # text
  labs(
    title = toupper(title),
    subtitle = st,
    caption = social
  ) +
  # fix coordinates
  coord_fixed() +
  # styling
  theme_void(base_size = 30, base_family = "SpaceMono") +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    # text styling
    plot.title = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      colour = highlight_col,
      size = 48,
      face = "bold",
      lineheight = 0.5,
      family = "SpaceMono",
      margin = margin(b = 10, t = 5)
    ),
    plot.subtitle = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      colour = text_col,
      minwidth = 1.5,
      size = 21,
      lineheight = 0.5,
      family = "SpaceMono",
      margin = margin(b = 5, t = 5)
    ),
    plot.caption = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      colour = text_col,
      size = 18,
      lineheight = 0.5,
      family = "SpaceMono",
      margin = margin(b = 5, t = 5)
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", date_chr, paste0(date_strip, ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
