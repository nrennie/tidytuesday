# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(snakecase)
library(ggchicklet)
library(ggimage)
library(cropcircles)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-03-19")
mutant_moneyball <- tuesdata$mutant_moneyball


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "ubuntu")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#000033"
text_col <- "#fee706"
highlight_col <- "#ef221c"

body_font <- "ubuntu"
title_font <- "ubuntu"


# Data wrangling ----------------------------------------------------------

plot_data <- mutant_moneyball |>
  mutate(
    Name = snakecase::to_title_case(Member),
    ValuePerIssue = TotalValue_ebay / TotalIssues,
    Name = reorder(Name, ValuePerIssue, decreasing = FALSE)
  ) |>
  select(Member, Name, ValuePerIssue) |>
  arrange(desc(ValuePerIssue)) |>
  slice_head(n = 10) |>
  mutate(
    img_path = file.path("2024", "2024-03-19", "images", paste0(Member, ".png")),
    img_circ = crop_circle(
      img_path,
      border_size = 16,
      border_colour = text_col
    )
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-03-19", "recording"),
  device = "png",
  width = 7,
  height = 5,
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
st <- "The trend of buying comics based on value speculation crippled the entire 
comic book industry for over a decade in the late 1990’s and early 2000’s. Here, the top 
10 most valueable X-Men characters are defined by their average value per issue. The 
total value of each X-Men team member's total number of issues as reflected by 
eBay sales in 2022 in which sellers tagged the issue as VG (Very Good) Condition is 
divided by the total number of issues each X-Men member appeared in between 1963 and 1992."
cap <- paste0(
  st,
  "<br>**Data**: rallyrd.com | **Images**:Wikipedia<br><br>",
  social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(
    x = Name,
    y = ValuePerIssue
  )
) +
  # bar chart
  geom_col(
    mapping = aes(
      y = ValuePerIssue / 2
    ),
    fill = highlight_col,
  ) +
  geom_chicklet(
    fill = highlight_col,
    colour = NA,
    radius = grid::unit(5, "mm")
  ) +
  geom_text(
    mapping = aes(
      x = Name,
      y = ValuePerIssue - 65,
      label = paste0("$", round(ValuePerIssue))
    ),
    colour = text_col,
    family = body_font,
    fontface = "bold",
    size = 8,
    hjust = 1
  ) +
  # add images
  geom_image(
    mapping = aes(y = ValuePerIssue - 30,
                  image = img_circ),
    size = 0.08
  ) +
  # add logo
  geom_image(
    data = slice_head(plot_data, n = 1),
    aes(
      x = 7,
      y = 600,
      image = "2024/images/logo.png"
    ),
    size = 0.5
  ) +
  # add text
  labs(tag = cap, x = "", y = "") +
  # scales
  coord_flip() +
  scale_y_continuous(expand = expansion(0, 0)) +
  # styling
  theme_minimal(base_size = 30, base_family = body_font) +
  theme(
    plot.margin = margin(15, 15, -10, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag.position = c(0.97, 0.3),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font,
      size = rel(0.8),
      maxwidth = 0.7
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = text_col),
    panel.grid = element_blank()
  )

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-03-19", paste0("20240319", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
