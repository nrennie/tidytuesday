library(tidyverse)
library(ggforce)
library(extrafont)
library(cowplot)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load('2021-07-13')
scoobydoo <- tuesdata$scoobydoo

#circle plot
daphnie_rad <- sqrt(sum(as.numeric(scoobydoo$caught_daphnie == "TRUE"))/pi)
fred_rad <- sqrt(sum(as.numeric(scoobydoo$caught_fred == "TRUE"))/pi)
shaggy_rad <- sqrt(sum(as.numeric(scoobydoo$caught_shaggy == "TRUE"))/pi)
velma_rad <- sqrt(sum(as.numeric(scoobydoo$caught_velma == "TRUE"))/pi)
scooby_rad <- sqrt(sum(as.numeric(scoobydoo$caught_scooby == "TRUE"))/pi)

circles <- data.frame(
  x0 = c(-30, -15, 0, 15, 30),
  y0 = rep(0,5),
  r = c(daphnie_rad, fred_rad, shaggy_rad, velma_rad, scooby_rad),
  char_name=c("daphne","fred", "shaggy", "velma", "scooby")
)

texts1 <- data.frame(
  x=c(-30, -15, 0, 15, 30), 
  y= rep(10,5), 
  label = c("Daphne","Fred", "Shaggy", "Velma", "Scooby")
)

texts2 <- data.frame(
  x=c(-30, -15, 0, 15, 30), 
  y= rep(-10,5), 
  label = c(sum(as.numeric(scoobydoo$caught_daphnie == "TRUE")), 
                sum(as.numeric(scoobydoo$caught_fred == "TRUE")), 
                    sum(as.numeric(scoobydoo$caught_shaggy == "TRUE")), 
                        sum(as.numeric(scoobydoo$caught_velma == "TRUE")), 
                            sum(as.numeric(scoobydoo$caught_scooby == "TRUE")))
)

texts3 <- data.frame(
  x=rep(as.Date("2022-01-01"),5), 
  y= c(29,132,77,41,160), 
  label = c("Daphne","Fred", "Shaggy", "Velma", "Scooby")
)

#line plot
monsters_caught <- tibble(date_aired = scoobydoo$date_aired, 
                          daphne = cumsum(as.numeric(scoobydoo$caught_daphnie == "TRUE")),
                          fred = cumsum(as.numeric(scoobydoo$caught_fred == "TRUE")), 
                          shaggy = cumsum(as.numeric(scoobydoo$caught_shaggy == "TRUE")), 
                          velma = cumsum(as.numeric(scoobydoo$caught_velma == "TRUE")), 
                          scooby = cumsum(as.numeric(scoobydoo$caught_scooby == "TRUE")))
plot_data <- pivot_longer(monsters_caught, cols=2:6, values_to = "num", names_to = "caught_by")

#make plots
p1 <- ggplot() + 
  geom_circle(data = circles, aes(x0 = x0, y0 = y0, r = r, fill = char_name, colour=char_name)) +
  geom_text(data = texts1, mapping=aes(x=x, y=y, label=label), colour="#7b50a0", hjust=0.5, family="Forte", size=5) +
  geom_text(data = texts2, mapping=aes(x=x, y=y, label=label), colour="#7b50a0", hjust=0.5, family="Forte", size=5) +
  xlim(-40,40) + ylim(-10,30) +
  coord_fixed() +
  scale_colour_manual("", values=c("fred" = "#009edb", "scooby" = "#ae6e0c", "velma" = "#f7971c", "shaggy" = "#b0ba1b", "daphne" = "#7867ae")) +
  scale_fill_manual("", values=c("fred" = "#009edb", "scooby" = "#ae6e0c", "velma" = "#f7971c", "shaggy" = "#b0ba1b", "daphne" = "#7867ae")) +
  theme(plot.background = element_rect(fill = "#d2e26b", colour="#d2e26b"),
        panel.background = element_rect(fill = "#d2e26b", colour="#d2e26b"),
        legend.position="none",
        plot.title = element_text(colour = "#7b50a0", size=28, hjust = 0.5, family="Forte"),
        plot.subtitle = element_text(colour = "#7b50a0", size=16, hjust = 0.5, family="Maiandra GD"),
        plot.caption = element_text(colour = "#7b50a0", size=12, hjust = 0.5, family="Maiandra GD"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-0.5, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1


p2 <- ggplot() + 
  geom_line(data=plot_data, mapping = aes(x=date_aired, y=num, colour=caught_by)) +
  labs(y="Nummber of monsters caught\n") +
  xlim(as.Date("1968-01-01"), as.Date("2030-01-01")) +
  geom_text(data = texts3, mapping=aes(x=x, y=y, label=label, colour=label), hjust=0, family="Maiandra GD", size=5) +
  scale_colour_manual("", values=c("fred" = "#009edb", "scooby" = "#ae6e0c", "velma" = "#f7971c", "shaggy" = "#b0ba1b", "daphne" = "#7867ae", "Fred" = "#009edb", "Scooby" = "#ae6e0c", "Velma" = "#f7971c", "Shaggy" = "#b0ba1b", "Daphne" = "#7867ae")) +
  scale_fill_manual("", values=c("fred" = "#009edb", "scooby" = "#ae6e0c", "velma" = "#f7971c", "shaggy" = "#b0ba1b", "daphne" = "#7867ae")) +
  theme(plot.background = element_rect(fill = "#d2e26b", colour="#d2e26b"),
        panel.background = element_rect(fill = "#d2e26b", colour="#d2e26b"),
        legend.position="none",
        plot.title = element_text(colour = "#7b50a0", size=28, hjust = 0.5, family="Forte"),
        plot.subtitle = element_text(colour = "#7b50a0", size=12, hjust = 0.5, family="Maiandra GD"),
        plot.caption = element_text(colour = "#7b50a0", size=12, hjust = 0.5, family="Maiandra GD"),
        axis.text = element_text(colour = "#7b50a0", size=12, hjust = 0.5, family="Maiandra GD"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "#7b50a0", size=12, hjust = 0.5, family="Maiandra GD"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_line(colour=alpha("#7b50a0", 0.4)),
        panel.grid.minor = element_blank())
p2

p <- p1 + p2 + plot_layout(nrow=1) + 
  plot_annotation(title = "\nScooby Doo, Where are you!", 
                  caption = "N. Rennie | Data: Kaggle | Images: Wikipedia", 
                  subtitle="Across 603 Scooby Doo storylines, it was Scooby who caught the most monsters.\nScooby has always been better at catching monsters, but second-place Fred\nonly improved in the 2010s. \n\nMaybe dogs are smarter than humans...") & 
  theme(plot.background = element_rect(fill = "#d2e26b", colour="#d2e26b"),
        panel.background = element_rect(fill = "#d2e26b", colour="#d2e26b"),
        plot.title = element_text(colour = "#7b50a0", size=28, hjust = 0.5, family="Forte"),
        plot.subtitle = element_text(colour = "#7b50a0", size=16, hjust = 0.5, family="Maiandra GD"),
        plot.caption = element_text(colour = "#7b50a0", size=12, hjust = 0, family="Maiandra GD"),)
p

daphne <- "./images/daphne.png"
fred <- "./images/fred.png"
velma <- "./images/velma.png"
shaggy <- "./images/shaggy.png"
scooby <- "./images/scooby.png"
machine <- "./images/machine.png"


q <- ggdraw() + 
  draw_plot(p) +
  draw_image(daphne, x = 0.09, y = 0.54, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.04) +
  draw_image(fred, x = 0.17, y = 0.535, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.04) +
  draw_image(shaggy, x = 0.24, y = 0.54, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.04) +
  draw_image(velma, x = 0.315, y = 0.54, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.04) +
  draw_image(scooby, x = 0.39, y = 0.54, hjust = 0.5, vjust = 1, halign = 1, valign = 1, width = 0.04)
q
