# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggbeeswarm)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-09-10")
college_admissions <- tuesdata$college_admissions


# Load fonts --------------------------------------------------------------

font_add_google("Montserrat", "Montserrat")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#F1F0EA"
text_col <- "#0E0F19"
col_palette <- c("#D1495B", "#EDAE49", "#00798C")
highlight_col <- col_palette[3]

body_font <- "Montserrat"
title_font <- "Montserrat"


# Data wrangling ----------------------------------------------------------

plot_data <- college_admissions |>
  select(par_income_lab, tier, rel_att_cond_app) |> 
  drop_na() |> 
  mutate(
    tier = case_when(
      tier %in% c("Ivy Plus", "Other elite schools (public and private)") ~ 
        "Ivy League and other elite schools",
      tier %in% c("Highly selective public", "Highly selective private") ~ 
        "Highly selective",
      tier %in% c("Selective public", "Selective private") ~ 
        "Selective"
    ),
    tier = factor(tier, levels = c(
      "Ivy League and other elite schools",
      "Highly selective",
      "Selective"
    ))
  ) |> 
  mutate(
    par_income_lab = factor(par_income_lab),
    par_income_lab = fct_relevel(par_income_lab, "Top 0.1", after = Inf)
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-09-10", "recording"),
  device = "png",
  width = 5,
  height = 8,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  twitter = NA,
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Economic Diversity and Student Outcomes"
st <- glue("Higher parental income appears to be somewhat correlated with 
higher collee acceptance rates, especially at the 
top end of parental income, though variability also increases. 
Generally, a lower acceptance rate is seen for <span style='color:{col_palette[1]}'>**Ivy League 
and other elite schools**</span> compared to <span style='color:{col_palette[2]}'>**highly selective**</span> 
and <span style='color:{col_palette[3]}'>**selective**</span> schools - but this 
trend does not persist at high levels of parental income.")
cap <- paste0(
  "**Data**: Opportunity Insights<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(
    x = rel_att_cond_app,
    y = par_income_lab,
    colour = tier
  )
) +
  geom_beeswarm(size = 0.5, cex = 0.6, alpha = 0.7) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(
    values = col_palette
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = "Ratio of relative attendance rate to relative application rate",
    y = "Parent\nIncome\n(Percentile)\n"
  ) +
  theme_minimal(base_size = 22, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      face = "bold",
      size = rel(1.7),
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.3,
      colour = alpha(text_col, 0.3)
    ),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(
      colour = text_col
    ),
    axis.title.y = element_text(
      angle = 0, lineheight = 0.2,
      hjust = 1,
      vjust = 1.01,
      margin = margin(r = -25, b = 20),
      colour = text_col,
      size = rel(0.9)
    ),
    axis.text.x = element_text(
      margin = margin(t = -5),
      colour = text_col
    ),
    axis.title.x = element_text(
      colour = text_col,
      size = rel(0.9)
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-09-10", paste0("20240910", ".png")),
  width = 5,
  height = 8
)

gg_playback(
  name = file.path("2024", "2024-09-10", paste0("20240910", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
