library(tidyverse)
library(showtext)
library(usefunc)
library(lubridate)
library(jcolors)
library(gtable)
library(grid)
library(RCurl)
library(png)

tuesdata <- tidytuesdayR::tt_load('2022-03-15')
cran <- tuesdata$cran

# add fonts
font_add_google(name = "Source Code Pro", family = "source")
showtext_auto()

# list tidyverse packages
tv_pkg <- c("dplyr", "forcats", "ggplot2", "purrr", "readr", "stringr", "tibble", "tidyr")

# prep data
con_char_date <- function(date){
  k <- str_split(date, pattern = " ")[[1]]
  if (length(k) == 5){
    return(mdy(str_flatten(k[c(2, 3, 5)], collapse = " ")))
  }
  if (length(k) == 6){
    return(mdy(str_flatten(k[c(2, 4, 6)], collapse = " ")))
  }
}

con_date <- function(date){
  tryCatch(as.Date(date), error=function(e) con_char_date(date))
}

convert_date <- function(date_vec){
  unlist(lapply(date_vec, function(x) con_date(x)))
}

plot_data <- cran %>% 
  filter(package %in% tv_pkg, 
         rmd > 0) %>% 
  mutate(date = convert_date(date), 
         date = as_date(date)) 

# plot
p <- ggplot() +
  geom_area(data = plot_data, 
            mapping = aes(x = date, y = rmd, group = package, fill = package)) +
  facet_wrap(~package, nrow = 1) +
  scale_x_date(limits = c(ymd(20140101), ymd(20211231)),
               breaks = c(ymd(20140101), ymd(20180101)),
               labels = c(2014, 2018)) +
  scale_y_continuous(limits = c(0, 15), 
                     breaks = c(0, 5, 10)) +
  coord_cartesian(expand = F) +
  scale_fill_jcolors(palette = "pal6") +
  labs(x = "", 
       y = "Number of .Rmd vignettes", 
       title = "{tidyverse} vignettes", 
       subtitle = str_wrap_break("The tidyverse is a collection of open source R packages that `share an underlying design philosophy, grammar, and data structures of tidy data`. Of the 8 core tidyverse packages {dplyr} has the highest number of vignettes.\n\nN. Rennie | Data: github.com/rmflight/vignette_analysis\n\n", 70)) +
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "#3A3B3C", fill = "#3A3B3C"), 
        panel.background = element_rect(colour = "#3A3B3C", fill = "#3A3B3C"), 
        plot.title = element_text(colour = "white", family = "source", face = "bold", hjust = 0, size = 16), 
        plot.subtitle = element_text(colour = "white", family = "source", hjust = 0, size = 12, 
                                     margin = margin(5,5,10,5)),
        axis.text = element_text(colour = "white", family = "source", hjust = 0, size = 12),
        axis.title = element_text(colour = "white", family = "source", hjust = 0.5, size = 12),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), 
        panel.spacing = unit(1.5, "lines"),
        strip.background = element_rect(colour = "#3A3B3C", fill = "#3A3B3C"),
        strip.text = element_text(margin = margin(20,5,20,5)),
        panel.grid.major = element_line(colour = "#4d4e4f"),
        panel.grid.minor = element_line(colour = "#4d4e4f"))
p

# add images
dplyr <- readPNG("dplyr.png")
forcats <- readPNG("forcats.png")
ggplot2 <- readPNG("ggplot2.png")
purrr <- readPNG("purrr.png")
readr <- readPNG("readr.png")
stringr <- readPNG("stringr.png")
tibble <- readPNG("tibble.png")
tidyr <- readPNG("tidyr.png")

# edit facet labels
g <- ggplot_gtable(ggplot_build(p))
strips <- grep("strip", g$layout$name)
new_grobs <- list(rasterGrob(dplyr),
                  rasterGrob(forcats), 
                  rasterGrob(ggplot2), 
                  rasterGrob(purrr), 
                  rasterGrob(readr), 
                  rasterGrob(stringr), 
                  rasterGrob(tibble), 
                  rasterGrob(tidyr))
g$grobs[strips] <- new_grobs
grid.draw(g)

