library(attachment)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

# utils function
str_extract_between <- function(x, start, end) {
  pattern <- paste0("(?<=", start, ")(.*?)(?=", end, ")")
  return(stringr::str_extract(x, pattern = pattern))
}

# function to get imports from python scripts
att_from_pyscript <- function(file) {
  file_txt <- readLines(file)
  import_lines <- file_txt[1:(which(file_txt == "")[1] - 1)]
  output <- character(length = length(import_lines))
  for (i in 1:length(import_lines)) {
    if (stringr::str_starts(import_lines[i], "import")) {
      output[i] <- stringr::str_extract(import_lines[i], "(?<=import )(\\w+)")
    } else if (stringr::str_starts(import_lines[i], "from")) {
      output[i] <- stringr::str_extract(import_lines[i], "(?<=from )(\\w+)")
    }
  }
  output <- stringr::str_flatten_comma(output)
  return(output)
}

# get list of all #tidytuesday folders
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
  filter(year %in% c(2019, 2020, 2021, 2022, 2023, 2024, 2025)) |>
  drop_na(week) |>
  mutate(
    title = NA_character_,
    pkgs = NA_character_,
    code_fpath = NA_character_,
    img_fpath = NA_character_,
    code_type = NA_character_
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
    subset(stringr::str_detect(tt_imgs, stringr::str_remove_all(tt_week$week, "-|_")))
  all_weeks[i, "img_fpath"] <- tt_imgs[1]
  
  # get all packages used
  tt_file <- list.files(file.path(tt_week$year, tt_week$week),
    pattern = ".R|.py|.svelte|.js", full.names = TRUE
  )[1]
  all_weeks[i, "code_fpath"] <- tt_file
  if (stringr::str_detect(tt_file, ".js")) {
    all_weeks[i, "pkgs"] <- "D3"
    all_weeks[i, "code_type"] <- "JavaScript"
  } else if (stringr::str_detect(tt_file, ".R")) {
    tt_pkgs <- att_from_rscript(tt_file) |>
      stringr::str_flatten_comma()
    all_weeks[i, "pkgs"] <- tt_pkgs
    all_weeks[i, "code_type"] <- "R"
  } else if (stringr::str_detect(tt_file, ".py")) {
    tt_pkgs <- att_from_pyscript(tt_file)
    all_weeks[i, "pkgs"] <- tt_pkgs
    all_weeks[i, "code_type"] <- "Python"
  } else if (stringr::str_detect(tt_file, ".svelte")) {
    all_weeks[i, "pkgs"] <- "SveltePlot"
    all_weeks[i, "code_type"] <- "JavaScript"
  }
  
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
readr::write_csv(all_weeks, "data/all_weeks.csv")
save(all_weeks, file = "data/all_weeks.RData")
