# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggfx)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-12-02")
sechselaeuten <- tuesdata$sechselaeuten


# Load fonts --------------------------------------------------------------

font_add("Poppins",
  regular = "fonts/Poppins/Poppins-Regular.ttf",
  italic = "fonts/Poppins/Poppins-Italic.ttf",
  bold = "fonts/Poppins/Poppins-Bold.ttf",
  bolditalic = "fonts/Poppins/Poppins-BoldItalic.ttf"
)
font_add("FA",
  regular = "fonts/Font Awesome 6 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Poppins"
body_font <- "Poppins"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "cornflowerblue"


# Data wrangling ----------------------------------------------------------

plot_data <- sechselaeuten |>
  drop_na(duration, tre200m0)

plot_data <- plot_data |>
  mutate(
    correct =
      duration > median(plot_data$duration) & tre200m0 < median(plot_data$tre200m0) | duration < median(plot_data$duration) & tre200m0 > median(plot_data$tre200m0)
  )

perc <- round(100 * sum(plot_data$correct) / nrow(plot_data))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-12-02", "recording"),
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
icon <- "<span style='font-size:8pt;font-family:\"FA\";color:#3C598E;'>&#xf111;</span> "
title <- "Can an exploding snowman predict the summer season?"
st <- glue("The Böögg is a snowman effigy made of cotton wool and stuffed with fireworks, created every year for Zurich's *Sechselaeuten* spring festival. The saying goes that the quicker the Böögg's head explodes, the finer the summer will be. The Böögg was {icon}<span style='color: #3C598E;'>**correct**</span>* only {perc}% of the time, so maybe just flip a coin?")
cap <- paste0(
  "**Note**: *Correct is defined as above or below median burning duration predicting below or above median temperatures.<br>**Data**: data.geo.admin.ch | sechselaeuten.ch<br>**Graphic**: ", social
)

longest_burn <- plot_data |>
  slice_max(duration)
hottest <- plot_data |>
  slice_max(tre200m0)

annot1 <- glue("**Longest burn time**<br>{longest_burn$duration} minutes in {longest_burn$year}")
annot2 <- glue("**Hottest summer**<br>{hottest$tre200m0}°C on average in {hottest$year}")

annot_data <- tribble(
  ~x, ~y, ~label,
  54, 16.7, annot1,
  16, 21.2, annot2
)

# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  annotate(
    "rect",
    xmin = median(plot_data$duration), xmax = Inf,
    ymin = median(plot_data$tre200m0), ymax = Inf, alpha = 0.5,
    fill = "grey"
  ) +
  annotate(
    "text",
    x = 60, y = 21.5, label = "Slow burn, hot summer",
    colour = "grey30", family = body_font, fontface = "bold.italic",
    hjust = 1.05
  ) +
  annotate(
    "rect",
    xmin = median(plot_data$duration), xmax = Inf,
    ymin = -Inf, ymax = median(plot_data$tre200m0), alpha = 0.5,
    fill = highlight_col
  ) +
  annotate(
    "text",
    x = 60, y = 15.5, label = "Slow burn, cool summer",
    colour = "#3C598E", family = body_font, fontface = "bold.italic",
    hjust = 1.05
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = median(plot_data$duration),
    ymin = median(plot_data$tre200m0), ymax = Inf, alpha = 0.5,
    fill = highlight_col
  ) +
  annotate(
    "text",
    x = 0.5, y = 20.8, label = "Fast\nburn,\nhot\nsummer",
    colour = "#3C598E", family = body_font, fontface = "bold.italic",
    hjust = 0
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = median(plot_data$duration),
    ymin = -Inf, ymax = median(plot_data$tre200m0), alpha = 0.5,
    fill = "grey"
  ) +
  annotate(
    "text",
    x = 0.5, y = 16.2, label = "Fast\nburn,\ncool\nsummer",
    colour = "grey30", family = body_font, fontface = "bold.italic",
    hjust = 0
  ) +
  with_outer_glow(
    geom_point(
      mapping = aes(
        x = duration, y = tre200m0,
        shape = correct, colour = correct
      ),
      size = 3
    ),
    colour = "white",
    expand = 3
  ) +
  scale_colour_manual(values = c("grey50", "#3C598E")) +
  geom_textbox(
    data = annot_data,
    mapping = aes(x = x, y = y, label = label),
    family = body_font,
    colour = text_col, fill = alpha(bg_col, 0.2),
    box.colour = "transparent"
  ) +
  scale_x_continuous(limits = c(0, 60)) +
  scale_y_continuous(limits = c(15, 22)) +
  labs(
    x = "Burning duration (minutes)",
    y = "Average air temperature 2m above ground (°C)",
    title = title, subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 20, 10, 10),
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
      margin = margin(b = 35, t = 5),
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
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(hjust = 1, margin = margin(t = 5)),
    axis.title.y.left = element_text(
      angle = 0, vjust = 1.07,
      family = body_font,
      size = rel(0.9),
      margin = margin(r = -215)
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-12-02", paste0("20251202", ".png")),
  width = 7,
  height = 6,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-12-02", paste0("20251202", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
