# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggforce)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-04-02")
dubois_week10 <- tuesdata$dubois_week10


# Load fonts --------------------------------------------------------------

font_add_google(name = "Space Mono", family = "space")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#e8dccd"
text_col <- "gray10"
highlight_col <- ""

flag_red <- "#e4141e"
flag_black <- "#000000"
flag_green <- "#00863d"

body_font <- "space"
title_font <- "space"


# Data wrangling ----------------------------------------------------------

plot_data <- dubois_week10 |>
  mutate(
    Occupation = str_to_upper(Occupation),
    Occupation = factor(Occupation, levels = Occupation),
    TotPerc = cumsum(Percentage)
  ) |>
  mutate(
    start = c(0, 2 * pi * (TotPerc / 100)[-6]) - pi/2,
    end = (2 * pi * (TotPerc / 100)) - pi/2
  ) |>
  mutate(
    leg1_x = rep(-2.4, 6),
    leg1_y = seq(0.5, -0.5, length.out = 6),
    leg2_x = rep(2.4, 6),
    leg2_y = seq(0.5, -0.5, length.out = 6)
  ) |> 
  mutate(
    label_r = case_when(
      Percentage < 5 ~ 0.92,
      TRUE ~ 0.5
    ),
    lab_x = label_r * sin((start + end) / 2),
    lab_y = label_r * cos((start + end) / 2),
    lab_label = paste0(Percentage, "%"),
    lab_size = case_when(
      Percentage < 5 ~ 3.5,
      TRUE ~ 6
    )
  )
plot_data$French_Occupation <- c(
  "PROFESSEURS ET INSTITUTEURS",
  "MINISTRES DE L'EVANGILE",
  "EMPLOYÉS DU GOUVERNMENT",
  "MARCHANDS",
  "MEDICINS,ADVOCATS,ET ÉTUDIANTS",
  "MÈRES DE FAMILLE"
)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-04-02", "recording"),
  device = "png",
  width = 5,
  height = 6.25,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

title_text <- str_to_upper(
  "A Series Of Statistical Charts Illustra-<br>ting The Conditions Of the Descendants Of For-<br>mer African Slaves Now Resident In The United<br>States of America."
)
title <- glue("<span></span> <span></span> <span style='font-weight: bold; font-size:36pt'>{title_text}</span><br><br><span style='color:{alpha(flag_red, 0.5)}; font-weight: normal;'><span></span> <span></span> UNE SÉRIE DE CARTES ET DIAGRAMMES STATISTIQUES MONTRANT LA<br>
CONDITION PRÉSENTE DES DESCENDANTS DES ANCIENS ESCLAVES AFRI-<br>
CAINS ACTUELLMENT ÉTABLIS DANS LES ETATS UNIS D'AMÉRIQUE.</span>")
st <- glue("THE UNIVERSITY WAS FOUNDED IN 1867. IT HAS INSTRUCTED 6000 NEGRO STUDENTS.<br>
<span style='color:{alpha(flag_red, 0.5)};'>ĽUNIVERSITE A ÉTÉ FONDÉE EN 1867. ELLE A DONNÉ L'INSTRUCTION A'8000 ÉTUDIANTS NEGRES.</span><br>
IT HAS GRADUATED 330 NEGROES AMONG WHOM ARE:<br>
<span style='color:{alpha(flag_red, 0.5)};'>ELLE A DÉLIVRE DES DIPLOMES A 330 NEGRES DONT:</span>")
cap <- "<span></span> <span></span> THE UNIVERSITY HAS 20 PROFESSORS AND INSTRUCTORS AND 250 STUDENTS AT PRESENT.<br>
<span></span> <span></span> IT HAS FIVE BUILDINGS. 60 ACRES OF CAMPUS, AND A LIBRARY OF 11,000 VOLUMES. IT AIMS TO RAISE<br>
AND CIVILIZE THE SONS OF THE FREEDMEN BY TRAINING THEIR MORE CAPABLE MEMBERS IN THE LIBER-<br>
AL ARTS ACCORDING TO THE BEST STANDARDS OF THE DAY.<br>
<span></span> <span></span> THE PROPER ACCOMPLISHMENT OF THIS WORK DEMANDS AN ENDOWMENT FUND OF $500,000.<br>
<span></span> <span></span> Ĺ UNIVERSITÉ A ACTUELLEMENT 20 PROFESSEURS ET INSTRUCTEURS ET 250 ÉTUDIANTS.<br>
<span></span> <span></span> ELLE EST COMPOSÉE DE CINC BÂTIMENTS. 60 ACRES (ENVIRON 26 HECTARES) DE TERRAIN SERVANT DE<br>
COUR ET DE CHAMP DE RÉCRÉATION, ET DUNE BIBLIOTHÈQUE CONTENANT 11,000 VOLUMES.<br>
<span></span> <span></span> SON BUT EST D'ÉLEVER ET DE CIVILISER LES FILS DES NEGRES AFFRANCHIS EN DONNANT AUX MIEUX<br>
DOUÉS UNE ÉDUCATION DANS LES ARTS LIBÉRAUX EN ACCORD AVEC LES IDÉES LES PLUS PROGRES -<br>
SISTES DE L'ÉPOQUE.<br>
<span></span> <span></span> L'ACCOMPLISSEMENT DE CETTE CEUVRE DEMANDE UNE DOTATION DE $500,000 (2,500,000 FRANCS)."


# Plot --------------------------------------------------------------------

ggplot() +
  geom_arc_bar(
    data = plot_data,
    aes(
      x0 = 0, y0 = 0, r0 = 0, r = 1,
      start = start,
      end = end,
      fill = Occupation
    ),
    color = text_col,
    linewidth = 0.1
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(
      x = lab_x,
      y = lab_y,
      label = lab_label,
      size = lab_size
    ),
    family = body_font,
  ) +
  scale_size_identity() +
  # left legend
  geom_point(
    data = plot_data,
    mapping = aes(
      x = leg1_x,
      y = leg1_y,
      fill = Occupation
    ),
    size = 5,
    pch = 21,
    color = text_col
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(
      x = leg1_x + 0.15,
      y = leg1_y,
      label = Occupation
    ),
    size = 4.5,
    color = text_col,
    family = body_font,
    hjust = 0
  ) +
  # right legend
  geom_point(
    data = plot_data,
    mapping = aes(
      x = leg2_x,
      y = leg2_y,
      fill = Occupation
    ),
    size = 5,
    pch = 21,
    color = text_col
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(
      x = leg2_x - 0.15,
      y = leg2_y,
      label = French_Occupation
    ),
    size = 4.5,
    color = alpha(flag_red, 0.5),
    family = body_font,
    hjust = 1
  ) +
  # colours
  scale_fill_manual(
    values = c(flag_red, "#7d8ab4", "#e3c0b1", "#d6beaa", "#94927f", "#efb75a")
  ) +
  # text
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_fixed() +
  theme_void(base_size = 24) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.4,
      family = body_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font,
      size = rel(0.85)
    ),
    plot.caption = element_textbox_simple(
      colour = alpha(text_col, 0.7),
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font,
      size = rel(0.75)
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-04-02", paste0("20240402", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
