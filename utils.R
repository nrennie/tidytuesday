# Packages ----------------------------------------------------------------

library(ggplot2)


# Load fonts --------------------------------------------------------------

sysfonts::font_add_google("Oswald")
sysfonts::font_add_google("Nunito")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Colours -----------------------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"


# Caption -----------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
source_caption <- function(source, sep = "<br>", graphic = social) {
  glue::glue(
    "**Data**: {source} {sep} **Graphic**: {graphic}"
  )
}



# Theme -------------------------------------------------------------------

theme_tt <- function() {
  theme_minimal(base_size = 10, base_family = body_font) +
    theme(
      plot.margin = margin(5, 5, 5, 5),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      plot.title = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 5, t = 5),
        family = title_font,
        face = "bold",
        size = rel(1.5)
      ),
      plot.subtitle = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 10, t = 5),
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
      panel.grid.minor = element_blank()
    )
}
