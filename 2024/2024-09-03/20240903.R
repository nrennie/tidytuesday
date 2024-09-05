# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggalluvial)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-09-03")
qname_levels_single_response_crosswalk <- tuesdata$qname_levels_single_response_crosswalk
stackoverflow_survey_questions <- tuesdata$stackoverflow_survey_questions
stackoverflow_survey_single_response <- tuesdata$stackoverflow_survey_single_response


# Load fonts --------------------------------------------------------------

font_add_google("Open Sans", "open")
showtext_auto()
body_font <- "open"


# Define colours ----------------------------------------------------------

bg_col <- "grey95"
text_col <- "grey10"
highlight_col <- "#f48024"


# Data wrangling ----------------------------------------------------------

q_data <- stackoverflow_survey_single_response |>
  select(years_code_pro, ai_select, ai_threat) |>
  drop_na(years_code_pro) |>
  mutate(
    years_code_pro = case_when(
      years_code_pro <= 5 ~ "Less than 5 years",
      years_code_pro > 5 & years_code_pro <= 10 ~ "5 - 10 years",
      years_code_pro > 10 & years_code_pro <= 20 ~ "10 - 20 years",
      years_code_pro > 20 ~ "Over 20 years",
    ),
    years_code_pro = factor(years_code_pro, levels = c(
      "Less than 5 years", "5 - 10 years", "10 - 20 years", "Over 20 years"
    ))
  ) |>
  count(years_code_pro, ai_select, ai_threat) |>
  drop_na(ai_select)

plot_data <- q_data |>
  left_join(
    filter(
      qname_levels_single_response_crosswalk, qname == "ai_select"
    ),
    by = c("ai_select" = "level")
  ) |>
  select(-c(qname, ai_select)) |>
  rename(ai_select = label) |>
  left_join(
    filter(
      qname_levels_single_response_crosswalk, qname == "ai_threat"
    ),
    by = c("ai_threat" = "level")
  ) |>
  select(-c(qname, ai_threat)) |>
  rename(ai_threat = label) |>
  mutate(
    ai_threat = replace_na(ai_threat, "Did not answer")
  ) |>
  mutate(
    ai_select = factor(ai_select, levels = c(
      "Yes", "No, but I plan to soon", "No, and I don't plan to"
    )),
    ai_threat = factor(ai_threat, levels = c(
      "Yes", "I'm not sure", "No", "Did not answer"
    ))
  )


plot_data |>
  select(years_code_pro, ai_threat, n) |>
  group_by(years_code_pro, ai_threat) |>
  summarise(n = sum(n)) |>
  ungroup() |>
  group_by(years_code_pro) |>
  mutate(
    tot_dev = sum(n),
    p = 100 * n / tot_dev
  ) |>
  ungroup() |>
  filter(ai_threat == "Yes")


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-09-03", "recording"),
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
  font_family = body_font,
  twitter = NA
)
title <- "Most developers don't believe AI is a threat to their current job."
st <- glue("The 2024 Stack Overflow Annual Developer Survey, conducted in May
2024, gathered responses from over 65,000 developers. Of those developers who responded with information
about their experience level and whether or not they had used AI,
<span style='color:{highlight_col};'>**developers with less than five years
of professional coding experience**</span>
are most likely to believe that AI is a threat to their current role with 9.6%
answering yes to this question. This compares to just 6.4% of developers with
over 20 years of experience.")
cap <- paste0(
  "**Data**: Stack Overflow Annual Developer Survey 2024<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  aes(
    axis1 = years_code_pro, axis2 = ai_select, axis3 = ai_threat,
    y = n
  )
) +
  geom_alluvium(aes(fill = years_code_pro), alpha = 0.9) +
  geom_stratum() +
  geom_text(
    stat = "stratum",
    mapping = aes(label = str_wrap(after_stat(stratum), 10)),
    family = body_font,
    size = 6,
    lineheight = 0.5
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_fill_manual(
    values = c(highlight_col, "grey10", "grey40", "grey70")
  ) +
  scale_x_discrete(
    limits = c("years_code_pro", "ai_select", "ai_threat"),
    labels = str_wrap(c(
      "Not including education, how many years have you coded professional?",
      "Do you currently use AI tools in your development process?",
      "Do you believe AI is a threat to your current role?"
    ), 18),
    position = "top"
  ) +
  coord_cartesian(expand = F) +
  theme_void(base_family = body_font, base_size = 22) +
  theme(
    legend.position = "none",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      face = "bold",
      size = rel(1.9),
      family = body_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
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
    axis.text.x = element_text(
      family = body_font,
      colour = text_col,
      lineheight = 0.4,
      margin = margin(b = 5)
    ),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col)
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-09-03", paste0("20240903", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

ggsave(
  file.path("2024", "2024-09-03", paste0("20240903", ".png")),
  height = 6,
  width = 7
)
