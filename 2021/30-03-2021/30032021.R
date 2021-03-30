library(tidyverse)
library(doBy)
library(cowplot)

sephora <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/sephora.csv')
ulta <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/ulta.csv')
allCategories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')
allShades <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')
allNumbers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allNumbers.csv')


#choose six companies
choose_brands <- c("FENTY BEAUTY by Rihanna", "Revlon", "bareMinerals", "L'Oréal", "Maybelline", "Origins")
d <- filter(allNumbers, brand %in% choose_brands)
all_hex <- unique(d$hex)

#geom_tile showing all possible colours
df <- data.frame(
  x = 1:length(all_hex),
  y = rep(1, length(all_hex)),
  z = all_hex
)
p1 <- ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = z)) +
  scale_fill_manual("", values=all_hex) +
  labs(title="\nImplicit Bias in Makeup Shades", subtitle = "Beauty brands often label foundation shades with sequential numbers, which\nimplicitly prioritises those at the beginning of the sequence. Most companies rank\nlighter shades of foundation higher, and therefore prioritise customers with lighter skin. \n\n These are the shades of foundation on sale from Fenty Beauty, Revlon bareMinerals,\nL'Oreal, Maybelline, and Origins:") +
  theme(plot.background = element_rect(fill = "lightpink"),
        panel.background = element_rect(fill = "lightpink"),
        legend.background = element_rect(fill = "lightpink"),
        plot.title = element_text(colour = "violetred3", size=18, face="bold", hjust=0.5),
        plot.subtitle = element_text(colour = "violetred3", size=11, hjust=0.5),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
p1

k <- sapply(1:6, function(x) filter(allNumbers, brand %in% choose_brands[x])[which.minn(filter(allNumbers, brand %in% choose_brands[x])$numbers, n = 10), c(1,6)])
long_data <- data.frame(brand=unlist(k[1,]), hex=unlist(k[2,]))
long_data$x <- 1:10
long_data$y <- rep(1,10)

p2 <- ggplot(long_data, aes(x, y, width=1, height=1)) +
  geom_tile(aes(fill = hex)) + 
  facet_wrap(~brand) +
  scale_fill_manual("", values=all_hex) +
  labs(subtitle = "\nCompanies put these ten shades first:\n", 
       caption="N. Rennie | Data: The Pudding") +
  theme(plot.background = element_rect(fill = "lightpink"),
        strip.background =element_rect(fill="violetred3"),
        strip.text = element_text(colour = 'white'),
        panel.background = element_rect(fill = "lightpink"),
        legend.background = element_rect(fill = "lightpink"),
        plot.title = element_text(colour = "violetred3", size=18, face="bold", hjust=0.5),
        plot.subtitle = element_text(colour = "violetred3", size=11, hjust=0.5),
        plot.caption = element_text(colour = "violetred3", size = 8, hjust=0),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
p2

#dev.new(width=7,height=6,unit="in", noRStudioGD = TRUE)

p <- plot_grid(p1,p2,ncol=1,nrow=2) + theme(plot.background = element_rect(fill="lightpink", color = "lightpink"))
p


