library(tidyverse)
library(showtext)

# load fonts
font_add_google("Roboto", "Roboto")
font_add_google("Roboto Slab", "Roboto Slab")
showtext_auto()

# read in data
owid_energy <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv")

# prep data
plot_data <- owid_energy |>
  filter(year %in% c(1960, 1980, 2000, 2020)) |>
  filter(!is.na(iso_code)) |> 
  select(country, year, coal_prod_per_capita) |>
  filter(coal_prod_per_capita != 0) |> 
  drop_na() |> 
  pivot_wider(values_from = coal_prod_per_capita,
              names_from = year) |> 
  drop_na() |> 
  mutate(across(`1960`:`2020`, ~ 100 * (.x - `1960`)/ `1960`)) |> 
  pivot_longer(cols = `1960`:`2020`,
               values_to = "coal_prod_per_capita",
               names_to = "year") |> 
  mutate(year = as.numeric(year))

# colours
bg_col <- "#ffffff"
dark_col <- "#555656"
highlight_col <- "#b20e10"
light_col <- "#bec0c2"

# text
st <- "Global demand for coal is at an all-time high, but looking at the changes in coal production
per capita show that the story of coal production is very different across the globe. In the last 
20 years, Indonesia has become a leading exporter of coal."
cap <- "Data: Our World in Data\nGraphic: N. Rennie"

# base R plot
png(filename = "2023/2023-06-06/20230606_base.png",
    width = 6, height = 4.5, units = "in", res = 140)
par(mar=c(5,5,7,2), family = "Roboto")
plot(0, xlab = "", ylab = "% change per capita since 1960",
     xlim = c(1960, 2020), ylim = c(-100,25000), type = "n",
     xaxt = "n", yaxt = "n")
abline(v = c(1970, 1990, 2010), col = "lightgrey")
abline(h = seq(0, 25000, 2500), col = "lightgrey")
abline(h = seq(0, 25000, 5000), col = dark_col)
for (i in unique(plot_data$country)){
  country_dat <- plot_data[which(plot_data$country == i), ]
  lines(country_dat$year, country_dat$coal_prod_per_capita,
        col = light_col, type = "l",
        lwd = 1.5)
}
ind_dat <- plot_data[which(plot_data$country == "Indonesia"), ]
lines(ind_dat$year, ind_dat$coal_prod_per_capita,
      col = highlight_col,
      type = "l",
      lwd = 2)
abline(v = c(1960, 1980, 2000, 2020), lwd = 2)
title(main = "Coal Production per Capita", adj = 0, family = "Roboto Slab", line = 5)
title(sub = st, adj = 0, family = "Roboto", line = -13)
title(sub = cap, adj = 0, family = "Roboto", line = 3.5)
axis(1, at = c(1960, 1980, 2000, 2020), lwd.ticks = 0)
axis(2, las = 2, lwd.ticks = 0)
text(x = 2015, y = 23000, labels="Indonesia", 
     col = highlight_col, family = "Roboto Slab")
dev.off()



