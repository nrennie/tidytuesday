library(tidyverse)
library(extrafont)
library(cowplot)
library(magick)

tuesdata <- tidytuesdayR::tt_load('2021-02-23')
employed <- tuesdata$employed

construction_women <- filter(employed, employed$industry == "Construction" & employed$race_gender == "Women")
construction_men <- filter(employed, employed$industry == "Construction" & employed$race_gender == "Men")
retail_women <- filter(employed, employed$industry == "Retail trade" & employed$race_gender == "Women")
retail_men <- filter(employed, employed$industry == "Retail trade" & employed$race_gender == "Men")

c_women <- aggregate(construction_women$employ_n, list(x = construction_women$year), sum)
c_men <- aggregate(construction_men$employ_n, list(x = construction_men$year), sum)
r_women <- aggregate(retail_women$employ_n, list(x = retail_women$year), sum)
r_men <- aggregate(retail_men$employ_n, list(x = retail_men$year), sum)

d <- data.frame(year=c_women[1], c_women=c_women[2], c_men=c_men[2], r_women=r_women[2], r_men=r_men[2])
colnames(d) <- c("year", "c_women", "c_men", "r_women", "r_men")

make_plot_year <- function(year_select){
  #retail
  d_retail <- data.frame(gender=c("FEMALE", "MALE"), number=t(filter(d, year==year_select)[,c(4,5)]))
  d_retail <- rbind(d_retail, c(gender="dummy_var", number=2*sum(d_retail$number)))
  d_retail$percentage <- as.numeric(d_retail$number)/sum(as.numeric(d_retail$number))
  #construction
  d_construction <- data.frame(gender=c("FEMALE", "MALE"), number=t(filter(d, year==year_select)[,c(2,3)]))
  d_construction <- rbind(c(gender="dummy_var1", number=0.5*sum(d_construction$number)), d_construction, c(gender="dummy_var2", number=1.5*sum(d_construction$number)))
  d_construction$percentage <- as.numeric(d_construction$number)/sum(as.numeric(d_construction$number))
  d_construction$gender <- factor(d_construction$gender, levels = c("dummy_var1", "FEMALE", "MALE", "dummy_var2"))
  p <- ggplot() +
    geom_bar(data=d_retail, aes(x="", y=percentage, fill=gender), width = 1, stat = "identity") +
    geom_bar(data=d_construction, aes(x="", y=percentage, fill=gender), width = 1, stat = "identity") +
    scale_fill_manual("", values=c("MALE" = "turquoise4", "FEMALE" = "orchid3", "Dummy_var_1"="white", "Dummy_var_2"="white"), limits = c('MALE', 'FEMALE')) +
    coord_polar("y", start=-45, clip = 'off') +
    labs(title="\nGENDER DIVIDES IN CONSTRUCTION AND\nRETAIL INDUSTRIES") +
    theme(panel.background = element_rect(fill = "lightsteelblue1"),
          plot.background = element_rect(fill = "lightsteelblue1"),
          legend.background = element_blank(),
          plot.title = element_text(colour = "navy", size=11, hjust = 0.5, face=2, family="Lucida Sans Typewriter"),
          legend.title = element_blank(),
          legend.position=c(1.2,0.5),
          legend.justification=c(1.2,0.5),
          plot.margin = unit(c(0.2, 0, 1.5, 0), "cm"), #top, right, bottom, left
          legend.key = element_rect(fill = "lightsteelblue1"),
          legend.spacing.x = unit(0.1,"cm"),
          legend.text = element_text(colour="navy", size=9, family="Lucida Sans Typewriter", margin = margin(r = 100, unit = "pt")),
          axis.title.x= element_blank(),
          axis.title.y= element_blank(),
          axis.ticks = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
    )
  q <- ggdraw(p) +
    draw_label("CONSTRUCTION", x = 0.5, y = 0.15, hjust = 0.5, vjust = 0, fontfamily = "Lucida Sans Typewriter", color = "navy", size = 10) +
    draw_label("RETAIL", x = 0.5, y = 0.85, hjust = 0.5, vjust = 0, fontfamily = "Lucida Sans Typewriter", color = "navy", size = 10) +
    draw_label(year_select, x = 0.3, y = 0.5, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = "navy", fontface=2, size = 14)
  save_plot(paste("p_",year_select,".jpg", sep=""), q, base_height = 6.25, base_width = 5)
}

sapply(d$year, function(x) make_plot_year(x))

#make gif
imgs <- list.files(full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 1)
img_animated
image_write(image = img_animated,path = "23022021.gif")

