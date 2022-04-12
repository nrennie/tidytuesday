library(tidyverse)
library(usefunc)
library(ggside)
library(hrbrthemes)
library(grid)
library(cowplot)

# load data
tuesdata <- tidytuesdayR::tt_load('2022-04-12')
indoor_pollution <- tuesdata$indoor_pollution
fuel_gdp <- tuesdata$fuel_gdp
fuel_access <- tuesdata$fuel_access
death_timeseries <- tuesdata$death_timeseries
death_source <- tuesdata$death_source
death_fuel <- tuesdata$death_fuel

# prep data
fuel_clean <- fuel_access %>%
  rename("access" = "Access to clean fuels and technologies for cooking  (% of population)") %>%
  select(-Code)
indoor_clean <- indoor_pollution %>%
  rename("deaths" = "Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)") %>%
  select(-Code)
plot_data <-
  left_join(fuel_clean, indoor_clean, by = c("Entity", "Year")) %>%
  drop_na()

# subtitle
st <- str_wrap_break("Indoor air pollution is a leading risk factor for premature death, with 4.1% of global deaths being attributed to indoor air pollution. There is an inverse relationship between access to clean fuels for cooking results and the percentage of deaths resulting from indoor air pollution.", 100)

# plot
p <- ggplot(data = plot_data,
            mapping = aes(x = access, y = deaths)) +
  geom_point(pch = 21, size = 1.5, alpha = 0.1,
             colour = "#3EBCD2", fill = "#3EBCD2") +
  geom_smooth(method = "gam", se = FALSE, colour = "#3785af", size = 2) +
  geom_xsidedensity(aes(y = stat(density)), colour = "#3EBCD2", fill="#3EBCD2", alpha = 0.5) +
  geom_ysidedensity(aes(x = stat(density)), colour = "#3EBCD2", fill="#3EBCD2", alpha = 0.5) +
  scale_xsidey_continuous(breaks = NULL, labels = "") +
  scale_ysidex_continuous(breaks = NULL, labels = "") +
  labs(title = "Indoor Air Pollution",
       subtitle = st,
       caption = "N. Rennie | Data: Our World in Data | 2000 - 2016 | #30DayChartChallenge",
       x = "% of population with access to clean fuels and technologies for cooking",
       y = "% of deaths resulting from household air pollution from solid fuels") +
  theme_ipsum_es(grid = "Y")
p

# red box
rect <- rectGrob(
  x = unit(0.75, "in"),
  y = unit(1, "npc"),
  width = unit(1, "in"),
  height = unit(0.25, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(col = "#E3120B", fill = "#E3120B", alpha = 1)
)

ggdraw(p) +
  draw_grob(rect)