# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggalluvial)
library(ggimage)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-04-29")
user2025 <- tuesdata$user2025


# Load fonts --------------------------------------------------------------

font_add_google("Source Sans 3", "source")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#FAFAFA"
text_col <- "black"
highlight_col <- "#2165b6"
null_col <- "#b3b3b3"

body_font <- "source"
title_font <- "source"


# Data wrangling ----------------------------------------------------------

plot_data <- user2025 |>
  mutate(co_authors = replace_na(co_authors, "")) |>
  mutate(
    Virtual = if_else(
      str_detect(session, "Virtual"), "Yes", "No, in person."
    ),
    Lightning = if_else(
      str_detect(session, "Lightning"), "Yes", "No, full length."
    ),
    Posit = if_else(
      str_detect(speakers, "Posit") | str_detect(co_authors, "Posit"),
      "Yes", "No, somewhere else."
    ),
    AI = if_else(
      str_detect(str_to_upper(title), "AI | LLM") | str_detect(str_to_upper(content), "AI | LLM"),
      "Yes", "No, doesn't mention it."
    )
  ) |>
  count(Virtual, Lightning, Posit, AI)



# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-04-29", "recording"),
  device = "png",
  width = 5,
  height = 8,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "useR! 2025"
st <- "useR! 2025 will be hosted at Duke University in Durham, NC, USA from August 8-10, 2025. The conference will feature keynote presentations from leading R developers and data scientists, technical talks and tutorials, interactive tutorials and training sessions, poster presentations, networking opportunities, and both in-person and virtual attendance options (with the virtual conference taking place on August 1, 2025)."
cap <- paste0(
  "**Data**: useR! 2025 Conference | **Graphic**:", social
)


# Plot --------------------------------------------------------------------

plot_function <- function(fill_category) {
  g <- ggplot(
    data = plot_data,
    aes(
      axis1 = AI, axis2 = Posit, axis3 = Lightning, axis4 = Virtual,
      y = n
    )
  ) +
    geom_alluvium(
      mapping = aes(fill = {{ fill_category }}),
      width = 1 / 6, reverse = FALSE, decreasing = FALSE,
      lode.guidance = "backward") +
    geom_stratum(width = 1 / 6, reverse = FALSE, decreasing = FALSE,
                 lode.guidance = "backward") +
    geom_text(
      stat = "stratum",
      mapping = aes(label = after_stat(stratum)),
      reverse = FALSE, decreasing = FALSE,
      lode.guidance = "backward"
    ) +
    geom_image(
      data = slice_head(plot_data, n = 1),
      aes(
        x = 4.9,
        y = -10,
        image = "2025/2025-04-29/logo.png"
      ),
      size = 0.1
    ) +
    scale_x_discrete(
      limits = c("AI", "Posit", "Lightning", "Virtual"),
      breaks = c("AI", "Posit", "Lightning", "Virtual"),
      labels = rev(str_wrap(c(
        "Is the talk virtual?",
        "Is it a lightning talk?",
        "Does a speaker work at Posit PBC?",
        "Does the title or abstract mention AI or LLMs?"
      ), 10))
    ) +
    scale_fill_manual(
      values = c(null_col, highlight_col)
    ) +
    labs(title = title, subtitle = st, caption = cap) +
    coord_flip(expand = FALSE, clip = "off") +
    theme_void(base_family = body_font) +
    theme(
      legend.position = "none",
      axis.text.y.left = element_text(
        hjust = 1,
        vjust = 0.2,
        face = "bold",
        margin = margin(r = -15)
      ),
      plot.margin = margin(5, 5, 5, 5),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      plot.title = element_textbox_simple(
        colour = text_col,
        hjust = 1,
        halign = 1,
        margin = margin(b = 0, t = 5),
        lineheight = 0.5,
        family = title_font,
        face = "bold",
        size = rel(1.8)
      ),
      plot.subtitle = element_textbox_simple(
        colour = text_col,
        hjust = 1,
        halign = 1,
        margin = margin(b = -100, t = 5),
        maxwidth = 0.8,
        family = body_font
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        hjust = 1,
        halign = 1,
        margin = margin(b = 0, t = 10, r = -2),
        family = body_font
      )
    )
  return(g)
}

plot_function(fill_category = Virtual)
ggsave(
  filename = file.path("2025", "2025-04-29", paste0("20250429", ".png")),
  height = 8,
  width = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

plot_function(fill_category = Lightning)
ggsave(
  filename = file.path("2025", "2025-04-29", paste0("20250429_lightning", ".png")),
  height = 8,
  width = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

plot_function(fill_category = Posit)
ggsave(
  filename = file.path("2025", "2025-04-29", paste0("20250429_posit", ".png")),
  height = 8,
  width = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

plot_function(fill_category = AI)
ggsave(
  filename = file.path("2025", "2025-04-29", paste0("20250429_ai", ".png")),
  height = 8,
  width = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2025", "2025-04-29", paste0("20250429", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
