# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(ggsankey)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-14")
house <- tuesdata$diwali_sales_data


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Rock Salt", "salt")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey10"
text_col <- "#fece2f"
highlight_col <- "grey40"

body_font <- "roboto"
title_font <- "salt"


# Data wrangling ----------------------------------------------------------

plot_data <- house |>
  select(User_ID, Zone, State, Gender, Marital_Status, `Age Group`) |>
  distinct() |> 
  mutate(Gender = 
           case_when(
             Gender == "M" ~ "Male",
             Gender == "F" ~ "Female"
           ),
         Marital_Status = 
           case_when(
             Marital_Status == 0 ~ "Single",
             Marital_Status == 1 ~ "Married"
           ))

State_levels <- plot_data |> 
  select(Zone, State) |> 
  distinct() |> 
  arrange(Zone) |> 
  pull(State)

sankey_data <- plot_data |>
  make_long(State, Zone, Gender, Marital_Status, `Age Group`) |> 
  mutate(node = factor(node),
         node = fct_relevel(node, State_levels))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-11-14", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Diwali Sales"
st <- "by customer segment"
cap <- paste0(
  "**Data**: Kaggle<br>", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = sankey_data,
  mapping = aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    label = node
  )
) +
  geom_sankey(flow.colour = text_col, flow.fill = highlight_col,
              flow.alpha = 0.6,
              linewidth = 0.1,
              node.fill = text_col, node.colour = text_col) +
  geom_sankey_label(size = 5, color = text_col, fill = "gray30",
                    width = 0.2) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 30) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 0, 5, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      size = 60,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = -20, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = -20),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-11-14", paste0("20231114", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
