# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggview)
library(PrettyCols)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-02-10")
schedule <- tuesdata$schedule


# Data wrangling ----------------------------------------------------------

plot_data <- schedule |>
  select(discipline_name,
    start_date = start_datetime_local,
    end_date = end_datetime_local, start_time, end_time
  ) |>
  mutate(
    start_date = as_datetime(as_date(start_date)),
    end_date = as_datetime(paste0(as_date(end_date), "23:59:59"))
  )


# Plot --------------------------------------------------------------------

p0 <- ggplot(data = plot_data) +
  geom_rect(
    mapping = aes(
      xmin = start_date, xmax = end_date,
      ymin = start_time, ymax = end_time,
      fill = discipline_name
    ),
    alpha = 0.5
  ) +
  scale_x_datetime() +
  scale_y_time()

p0 + 
  scale_fill_pretty_d("Velvet") +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none") +
  canvas(
    width = 5, height = 5,
    units = "in", bg = "#570F4F",
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-02-10", paste0("20260210", ".png"))
)
