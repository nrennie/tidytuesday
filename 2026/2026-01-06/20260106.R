# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(lubridate)
library(scales)
library(gh)
library(janitor)


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2026", "2026-01-06", "recording"),
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
  font_family = body_font,
  mastodon = NA
)
title <- "2025"
st <- "My jobs, talks and workshops, GitHub commits, and books read during the year."
cap <- paste0(
  "**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

## Work --------------------------------------------------------------------

plot_data <- data.frame(
  start = dmy(c("01012025", "17032025")),
  end = dmy(c("07032025", "31122025")),
  grp = c("LU", "ONS")
)
p1 <- ggplot() +
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = start, xmax = end,
      ymin = 0, ymax = 1,
      fill = grp
    )
  ) +
  scale_fill_manual(values = c(alpha(highlight_col, 0.6), highlight_col)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_nr() +
  theme_void() +
  theme(legend.position = "none")


## Talks -------------------------------------------------------------------

raw_data <- read_csv("2026/2026-01-06/data/cpd.csv")
plot_data <- raw_data |>
  filter(Year == 2025, Type == "Presentation / Workshop") |>
  select(Date) |>
  mutate(
    Date = mdy(Date),
    Week = week(Date)
  ) |>
  count(Week)
p2 <- ggplot() +
  annotate("segment",
    x = 1, xend = 52, y = 0.5, yend = 0.5,
    colour = highlight_col, linewidth = 0.5
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(x = Week, y = 0.5, size = n),
    colour = highlight_col
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_nr() +
  theme_void() +
  theme(legend.position = "none")



## GitHub ------------------------------------------------------------------

gh_gql("query { viewer { login }}")

# Function to get and parse data
get_data <- function(year) {
  # Get number of contributions by day
  contributions <- glue('query {
  user(login: "nrennie") {
    contributionsCollection(from: "[year]-01-01T00:00:00Z", to: "[year]-12-31T23:59:59Z") {
      contributionCalendar {
        weeks {
          contributionDays {
            date
            contributionCount
            color
          }
        }
      }
    }
  }
}', .open = "[", .close = "]")
  json_data <- gh_gql(contributions)
  # process data
  output <- json_data$data$user |>
    unlist() |>
    unname() |>
    matrix(ncol = 3, byrow = TRUE) |>
    as.data.frame() |>
    magrittr::set_colnames(c("date", "contributions", "colour")) |>
    as_tibble() |>
    select(date, contributions) |>
    mutate(
      contributions = as.numeric(contributions),
      date = ymd(date)
    ) |>
    mutate(
      weekday = wday(date),
      week = epiweek(date),
      year = year(date),
      week = if_else(
        week %in% c(52, 53) & month(date) == 1, 0, week
      ),
      week = if_else(
        week %in% c(1) & month(date) == 12, 53, week
      )
    )
  return(output)
}
data_2025 <- get_data(2025)
plot_data <- data_2025 |>
  group_by(week) |>
  summarise(n = sum(contributions)) |>
  ungroup() |>
  filter(week <= 50)
p3 <- ggplot() +
  geom_line(
    data = plot_data,
    mapping = aes(x = week, y = rescale(n)),
    colour = highlight_col,
    linewidth = 1
  ) +
  theme_nr() +
  theme_void() +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none")


## Books -------------------------------------------------------------------

raw_data <- read_csv("2026/2026-01-06/data/books.csv") |> clean_names()
plot_data <- raw_data |>
  filter(read_status == "read") |>
  select(title, authors, dates_read) |>
  separate_wider_delim(dates_read,
    delim = "-", names = c("start_date", "end_date"),
    too_few = "align_start"
  ) |>
  drop_na() |>
  mutate(
    start_date = ymd(start_date),
    end_date = ymd(end_date),
    start_year = year(start_date),
    end_year = year(end_date),
    raw_start_date = start_date,
    raw_end_date = end_date,
    days_to_read = as.numeric(raw_end_date - raw_start_date, unit = "days")
  ) |>
  filter(
    start_year %in% 2025,
    end_year %in% 2025
  ) |>
  mutate(
    start_date = ymd(glue("1900/{month(start_date)}/{mday(start_date)}")),
    end_date = ymd(glue("1900/{month(end_date)}/{mday(end_date)}")),
    col_start_year = as.character(start_year)
  )
p4 <- ggplot() +
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = start_date, xmax = end_date,
      ymin = 0, ymax = 1
    ),
    fill = highlight_col,
    alpha = 0.5,
    colour = "transparent"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_nr() +
  theme_void() +
  theme(legend.position = "none")



# Join --------------------------------------------------------------------

g <- p1 + p2 + p3 + p4 +
  plot_layout(ncol = 1)
g

# Plain version
gg_stop_recording()
g +
  plot_annotation(
    theme = theme(plot.margin = margin(5, -7, 5, -7))
  )
ggsave(
  filename = file.path("2026", "2026-01-06", paste0("20260106", ".png")),
  width = 7,
  height = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

# With text
g +
  plot_annotation(
    title = title,
    subtitle = st,
    caption = cap,
    theme = theme(
      plot.title = element_textbox_simple(
        colour = text_col,
        hjust = 0.5,
        halign = 0.5,
        margin = margin(b = 5, t = 5),
        family = title_font,
        width = 0.85,
        face = "bold",
        size = rel(1.6)
      ),
      plot.subtitle = element_textbox_simple(
        colour = text_col,
        hjust = 0.5,
        halign = 0.5,
        margin = margin(b = 5),
        family = body_font,
        width = 0.85,
        size = rel(0.8)
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        hjust = 0.5,
        halign = 0.5,
        margin = margin(t = 10),
        family = body_font,
        width = 0.85,
        size = rel(0.7)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
  )
ggsave(
  filename = file.path("2026", "2026-01-06", paste0("20260106_v2", ".png")),
  width = 7,
  height = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

