library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(geomtextpath)
library(ggforce)

# load data
drugs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv")


# Version 1 ---------------------------------------------------------------

# data wrangling
plot_data <- drugs |>
  filter(
    category == "veterinary",
    authorisation_status == "authorised",
    decision_date >= ymd("20180101")
  ) |>
  select(decision_date, species) |>
  mutate(
    Dogs = str_detect(species, "Dogs|dogs"),
    Cats = str_detect(species, "Cats|cats"),
    Chickens = str_detect(species, "Chicken|chicken"),
    Pigs = str_detect(species, "Pigs|pig")
  ) |>
  select(-species) |>
  pivot_longer(Dogs:Pigs) |>
  arrange(decision_date) |>
  filter(value) |>
  group_by(name) |>
  mutate(total = row_number()) |>
  drop_na() |> 
  mutate(name = factor(name, 
                       levels = c("Dogs", "Pigs", "Cats", "Chickens")))

# start recording
gg_record(
  dir = file.path("2023", "2023-03-14", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 10, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# text
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#F0F5F5;'>&#xf099;</span><span style='color:#2F4F4F;'>.</span><span style='font-family:Commissioner;color:#F0F5F5;'>@nrennie35</span><span style='color:#2F4F4F;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#F0F5F5;'>&#xf4f6;</span><span style='color:#2F4F4F;'>.</span><span style='font-family:Commissioner;color:#F0F5F5;'>fosstodon.org/@nrennie</span><span style='color:#2F4F4F;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#F0F5F5;'>&#xf09b;</span><span style='color:#2F4F4F;'>.</span><span style='font-family:Commissioner;color:#F0F5F5;'>nrennie</span><span style='color:#2F4F4F;'>..</span>"

st <- glue("The European Medicines Agency (EMA) is the official regulator that 
           directs drug development for both humans and animals, and decides
           whether to authorize marketing 
           a new drug in Europe or not. Medicines for dogs are being authorised
           at a faster rate compared to other animals including pigs,
           cats, and chickens.<br><br>Data: European Medicines Agency")


# plot
ggplot(
  data = plot_data,
  mapping = aes(x = decision_date, y = total, colour = (name == "Dogs"), group = name)
) +
  geom_textline(
    stat = "smooth", aes(label = name),
    hjust = 0.9,
    size = 12,
    linewidth = 0.8,
    fontface = "bold",
    family = "Commissioner"
  ) +
  labs(
    x = "",
    y = "Number of drugs authorised",
    title = "European Veterinary Drug Development",
    subtitle = st,
    caption = social
  ) +
  scale_colour_manual(values = c("#508080", "#E30B5C")) +
  scale_x_date(limits = ymd(c("20180101", "20220630"))) +
  scale_y_continuous(limits = c(0, 80)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 24, base_family = "Commissioner") +
  theme(
    plot.background = element_rect(fill = "#2F4F4F", colour = "#2F4F4F"),
    panel.background = element_rect(fill = "#2F4F4F", colour = "#2F4F4F"),
    plot.margin = margin(15, 15, 10, 10),
    text = element_text(
      family = "Commissioner",
      colour = "#F0F5F5",
      size = 28
    ),
    plot.title = element_text(
      family = "Fraunces",
      size = 60,
      hjust = 0,
      colour = "#F0F5F5"
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.4,
      family = "Commissioner",
      colour = "#F0F5F5",
      size = 34,
      margin = margin(t = 5, b = 10)
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.4,
      hjust = 0,
      size = 28,
      margin = margin(t = 10),
      family = "Commissioner",
      colour = "#F0F5F5"
    ),
    axis.text = element_text(
      colour = "#F0F5F5"
    ),
    panel.grid = element_line(
      colour = alpha("#508080", 0.4),
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    strip.text = element_text(colour = "#F0F5F5"),
    legend.position = "none"
  )


# Version 2 ---------------------------------------------------------------

# data wrangling
plot_data <- drugs |> 
  select(authorisation_status, condition_indication) |> 
  drop_na() |> 
  mutate(status = 
           case_when(
             authorisation_status == "authorised" ~ "Authorised",
             authorisation_status != "authorised" ~ "Refused / withdrawn"
           )) |> 
  mutate(condition_indication = tolower(condition_indication)) |> 
  mutate(`Covid-19` = str_detect(condition_indication,
                                 "covid-19|covid19|covid"),
         `Diabetes` = str_detect(condition_indication,
                                 "diabetes"),
         `HIV` = str_detect(condition_indication,
                            "hiv"),
         `Osteoporosis` = str_detect(condition_indication,
                                     "osteoporosis"),
         `Dementia` = str_detect(condition_indication,
                                 "dementia"),
         `Cystic Fibrosis` = str_detect(condition_indication,
                                        "cystic fibrosis")) |> 
  select(-c(condition_indication, authorisation_status)) |> 
  pivot_longer(-status, values_to = "is_drug", names_to = "Disease") |> 
  group_by(status, Disease) |> 
  summarise(total = sum(is_drug)) |> 
  ungroup() |> 
  group_by(Disease) |> 
  mutate(all_drugs = sum(total)) |> 
  mutate(prop = total/all_drugs) |> 
  select(-c(total, all_drugs))

gg_record(
  dir = file.path("2023", "2023-03-14", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4.5, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# text
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#0190c4;'>&#xf099;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>@nrennie35</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#0190c4;'>&#xf4f6;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>fosstodon.org/@nrennie</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#0190c4;'>&#xf09b;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>nrennie</span><span style='color:white;'>..</span>"

st <- glue("The European Medicines Agency (EMA) is the official regulator that 
           directs drug development for both humans and animals, and decides
           whether to authorize marketing  a new drug in Europe or not. The 
           infographic below shows the percentage of drugs used to treat six 
           conditions, which were <span style='color: #0190c4'>authorised</span>
           or <span style='color: #fdda49'>refused / withdrawn.</span>")

# plot
ggplot(data = plot_data) + 
  facet_wrap(~Disease, nrow = 2, ncol = 3) +
  geom_shape(data = data.frame(x = c(-0.2, 2.2, 2.2, -0.2, -0.2),
                               y = c(-0.2, -0.2, 1.2, 1.2, -0.2)),
             mapping = aes(x = x, y = y),
             fill = "grey93", 
             colour = "grey80",
             radius = unit(0.8, 'cm')
  ) +
  geom_shape(data = data.frame(x = c(0, 2, 2, 0, 0),
                               y = c(0, 0, 1, 1, 0)),
             mapping = aes(x = x, y = y),
             fill = "#fdda49", 
             radius = unit(0.7, 'cm')
  ) +
  geom_shape(data = data.frame(x = c(-0.02, 2.02, 2.02, -0.02, -0.02),
                               y = c(-0.02, -0.02, 1.02, 1.02, -0.02),
                               Disease = "Covid-19"),
             mapping = aes(x = x, y = y),
             fill = "#0190c4", 
             radius = unit(0.7, 'cm')
  ) +
  geom_shape(data = data.frame(x = c(-0.02, 1.02, 1.02, -0.02, -0.02),
                               y = c(-0.02, -0.02, 1.02, 1.02, -0.02)),
             mapping = aes(x = x, y = y),
             fill = "#0190c4", 
             radius = unit(0.7, 'cm')
  ) +
  geom_rect(data = filter(plot_data, status == "Authorised", Disease != "Covid-19"),
               mapping = aes(xmin = 0.5,
                             xmax = prop*2,
                             ymin = -0.02,
                             ymax = 1.02),
               fill = "#0190c4") +
  geom_text(data = filter(plot_data, status == "Authorised"),
            mapping = aes(x = 0.2,
                          y = 0.5,
                          label = Disease),
            colour = "white",
            hjust = 0,
            size = 9,
            fontface = "bold",
            family = "Commissioner") +
  labs(
    x = "",
    y = "",
    title = "European Drug Development",
    subtitle = st,
    caption = social
  ) +
  theme_minimal(base_size = 26, base_family = "Commissioner") +
  theme(
    plot.background = element_rect(fill = "gray95", colour = "gray95"),
    panel.background = element_rect(fill = "gray95", colour = "gray95"),
    plot.title = element_text(
      family = "Fraunces",
      size = 50,
      hjust = 0,
      colour = "gray5"
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.5,
      margin = margin(t = 5, b = 25),
      family = "Commissioner",
      colour = "gray5"
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.4,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(t = 5),
      family = "Commissioner",
      colour = "gray5"
    ),
    strip.text = element_blank(),
    plot.title.position = "plot",
    plot.margin = margin(10, 15, 10, 10),
    panel.spacing = unit(10, "pt"),
    axis.text = element_blank(),
    panel.grid = element_blank())


# Gif ---------------------------------------------------------------------

# save gif
gg_playback(
  name = file.path("2023", "2023-03-14", "20230314.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  image_resize = 800,
  frame_duration = .25,
  background = "white"
)
