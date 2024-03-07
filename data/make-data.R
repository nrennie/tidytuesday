library(attachment)
library(dplyr)
library(tidyr)
library(stringr)

# utils function
str_extract_between <- function(x, start, end) {
  pattern <- paste0("(?<=", start, ")(.*?)(?=", end, ")")
  return(stringr::str_extract(x, pattern = pattern))
}

# get list of all #tidytuesday folders
setwd("../tidytuesday")
all_folders <- tibble::tibble(
  folders = list.dirs(path = ".", recursive = TRUE)
)

# get list of all weeks
all_weeks <- all_folders |>
  mutate(folders = str_remove(folders, "./")) |>
  separate_wider_delim(
    folders,
    delim = "/",
    names = c("year", "week"),
    too_few = "align_start",
    too_many = "drop"
  ) |>
  filter(year %in% c(2020, 2021, 2022, 2023, 2024)) |>
  drop_na(week) |>
  mutate(
    title = NA_character_,
    pkgs = NA_character_,
    code_fpath = NA_character_,
    img_fpath = NA_character_
  )

# list file for each week
for (i in seq_len(nrow(all_weeks))) {
  # get week folder
  tt_week <- all_weeks[i, ]

  # get readme file and parse title
  if (tt_week$year == "2021") {
    tt_readme <- list.files(file.path(tt_week$year, tt_week$week, "/"),
      pattern = "\\.md|\\.MD", full.names = TRUE
    )
    readme_txt <- readLines(tt_readme, warn = FALSE)[2]
    readme_title <- readme_txt |>
      stringr::str_remove("</h1>") |>
      stringr::str_trim("both")
    all_weeks[i, "title"] <- readme_title
  } else {
    tt_readme <- list.files(file.path(tt_week$year, tt_week$week, "/"),
      pattern = "\\.md|\\.MD", full.names = TRUE
    )
    readme_txt <- readLines(tt_readme, warn = FALSE)[1]
    readme_title <- str_extract_between(
      readme_txt,
      start = ">", end = "<"
    ) |>
      stringr::str_trim("both")
    all_weeks[i, "title"] <- readme_title
  }

  # get image file path
  tt_imgs <- list.files(file.path(tt_week$year, tt_week$week, "/"),
    pattern = ".png|.PNG|.jpg|.JPG|.jpeg|.JPEG", full.names = TRUE
  )

  tt_imgs <- tt_imgs |>
    subset(stringr::str_detect(tt_imgs, stringr::str_remove_all(tt_week$week, "-")))
  all_weeks[i, "img_fpath"] <- tt_imgs[1]

  # get all packages used
  tt_file <- list.files(file.path(tt_week$year, tt_week$week, "/"),
    pattern = ".R", full.names = TRUE
  )[1]
  all_weeks[i, "code_fpath"] <- tt_file
  tt_pkgs <- att_from_rscript(tt_file) |>
    stringr::str_flatten_comma()
  all_weeks[i, "pkgs"] <- tt_pkgs
}

# packages to binary variables
binary_pkgs <- all_weeks |>
  select(week, pkgs) |>
  separate_longer_delim(pkgs, delim = ", ") |>
  mutate(value = 1) |>
  complete(week, pkgs) |>
  mutate(value = replace_na(value, 0)) |>
  unique() |>
  pivot_wider(names_from = pkgs, values_from = value)

# join
all_weeks <- all_weeks |>
  left_join(binary_pkgs, by = "week") |>
  distinct()

# save file
setwd("../tidytuesday-shiny-app")
writexl::write_xlsx(all_weeks, "data/all_weeks.xlsx")
save(all_weeks, file = "data/all_weeks.RData")
