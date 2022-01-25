library(tidyverse)
library(rcartocolor)
library(showtext)
library(cowplot)

# read in data
tuesdata <- tidytuesdayR::tt_load('2022-01-25')
ratings <- tuesdata$ratings
details <- tuesdata$details


# add fonts
font_add_google(name = "Bangers", family = "bangers")
font_add_google(name = "Saira", family = "saira")
showtext_auto()

# join data
details1 <- details %>% 
  select("id", "yearpublished")
ratings1 <- ratings %>% 
  filter(year > 0) %>% 
  select("id", "name", "average") 
set.seed(1234)
sqrt_games <- 5
plot_data <- left_join(ratings1, details1, by = "id") %>%
  filter(!is.na(yearpublished)) %>% 
  mutate(decade = as.character(yearpublished - yearpublished %% 10)) %>% 
  slice_sample(n = sqrt_games^2) %>% 
  mutate(group = 1:(sqrt_games^2)) 
plot_data$new_name <- separate(plot_data, name, into= c("new_name", "ex"), sep=":")$new_name

# prep plot
gen_square <- function(x, y, height, group){
  tibble(x = c(x, x+height, x+height, x),
             y = c(y, y, y+height, y+height),
             group = rep(group, 4))
}

squares <- tibble(expand.grid(x = 1:sqrt_games, y = 1:sqrt_games))
squares$heights <- rep(0.9, (sqrt_games^2))
plot_grid <- tibble(x = c(),
                        y = c(),
                        group = c(),
                        col = c())
n <- nrow(squares)
for (i in 1:n){
  k <- gen_square(x = squares$x[i],
                   y = squares$y[i],
                   height = squares$heights[i],
                   group = i)
  plot_grid <- rbind(plot_grid, k)
}

# join grid to data
final_data <- left_join(plot_grid, plot_data, by = "group")

#text data
label_data <- final_data %>%
  group_by(new_name) %>%
  slice_head(n = 1)

#legend data
legend_grid <- gen_square(x = 7,
                          y = 3,
                          height = 0.9,
                          group = 26)
legend_data <- left_join(legend_grid, 
                         (plot_data[1,c(1:7)] %>%
                           mutate(group = 26)), 
                         by = "group")

# plot 
p <- ggplot() + 
  geom_polygon(data = final_data, 
               mapping = aes(x = x, y = y, group = group, fill = decade, colour = average), 
               size = 2) + 
  geom_text(data = label_data, 
            mapping = aes(x = x + 0.45, y = y + 0.16, label = average), 
            colour = "white", fontface = 2, hjust = 0.5, family="saira") +
  geom_text(data = label_data, 
            mapping = aes(x = x + 0.45, y = y + 0.58, label = str_wrap(new_name, 13)), 
            colour = "black", size = 3, hjust = 0.5, family="saira") +
  #legend key
  geom_polygon(data = legend_data, 
               mapping = aes(x = x, y = y, group = group, fill = decade, colour = average), 
               size = 2) + 
  geom_text(data = legend_data[1,], 
            mapping = aes(x = x + 0.45, y = y + 0.16, label = average), 
            colour = "white", fontface = 2, hjust = 0.5, family="saira") +
  geom_text(data = legend_data[1,], 
            mapping = aes(x = x + 0.45, y = y + 0.58, label = str_wrap(new_name, 10)), 
            colour = "black", size = 3, hjust = 0.5, family="saira") +
  scale_fill_carto_d(palette = "Bold") +
  scale_color_gradient(low = "black", high = "white", limits = c(1,10), guide = "none") +
  labs(title = "Board Games", 
       subtitle = str_wrap("Board games date back to at least 3500BC, with Senet being played by Ancient Egyptians. In this dataset, the highest rated board game is Malhya: Lands of Legends with an average rating of 9.57 out of 10. A random sample of 25 games demonstrates the range of ratings and publication years.", 80), 
       caption = "N. Rennie | Data: Board Games Geek") +
  annotate("text", 
           x = 7.45, 
           y = 5.5,
           label = str_wrap("Inner colour represents decade of game published.", 26), 
           colour = "white", family="saira") +
  annotate("text", 
           x = 7.45, 
           y = 2,
           label = str_wrap("Outer colour represents average rating of game. Lighter coloured border indicates a higher rating.", 30), 
           colour = "white", family="saira") +
  coord_fixed() +
  theme_void() +
  xlim(1, 9) +
  ylim(1, 6) +
  guides(fill=guide_legend(ncol=3)) +
  theme(plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"),
        plot.title = element_text(colour = "white", size=36, hjust = 0.5, family="bangers", 
                                  margin = margin(0, 0, 10, 0)),
        plot.subtitle = element_text(colour = "white", size=12, hjust = 0.5, family="saira"),
        plot.caption = element_text(colour = "white", size=10, hjust = 0.5, family="saira"),
        legend.text = element_text(colour = "white", size=10, hjust = 0.5, family="saira"),
        legend.title = element_blank(),
        legend.position = c(0.78, 0.7),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# bg colour
q <- ggdraw() +
  draw_plot(p) + 
  theme(plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"))
q

dev.new(noRStudioGD = T, width = 8*1.3, height = 6*1.3, unit = "in")


#############################################################################################
#############################################################################################


plot_data1 <- left_join(ratings1, details1, by = "id") %>%
  filter(!is.na(yearpublished), 
         yearpublished > 1800) 
sqrt_games <- floor(sqrt(nrow(plot_data)))
plot_data <- plot_data1 %>% 
  mutate(decade = as.character(yearpublished - yearpublished %% 10)) %>% 
  mutate(group = 1:nrow(plot_data1)) 
plot_data$new_name <- separate(plot_data, name, into= c("new_name", "ex"), sep=":")$new_name
# prep plot
gen_square <- function(x, y, height, group){
  tibble(x = c(x, x+height, x+height, x),
         y = c(y, y, y+height, y+height),
         group = rep(group, 4))
}
squares <- tibble(expand.grid(x = 1:sqrt_games, y = 1:sqrt_games))
squares$heights <- rep(0.9, (sqrt_games^2))
plot_grid <- tibble(x = c(),
                    y = c(),
                    group = c(),
                    col = c())
n <- nrow(squares)
for (i in 1:n){
  k <- gen_square(x = squares$x[i],
                  y = squares$y[i],
                  height = squares$heights[i],
                  group = i)
  plot_grid <- rbind(plot_grid, k)
}
# join grid to data
final_data <- left_join(plot_grid, plot_data, by = "group")
# plot 
p <- ggplot() + 
  geom_polygon(data = final_data, 
               mapping = aes(x = x, y = y, group = group, fill = decade, colour = average), 
               size = 0.1) +
  scale_color_gradient(low = "black", high = "white", limits = c(1,10), guide = "none") +
  coord_fixed(expand = F) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"),
        plot.title = element_text(colour = "white", size=36, hjust = 0.5, family="bangers", 
                                  margin = margin(0, 0, 10, 0)),
        plot.subtitle = element_text(colour = "white", size=12, hjust = 0.5, family="saira"),
        plot.caption = element_text(colour = "white", size=10, hjust = 0.5, family="saira"),
        legend.text = element_text(colour = "white", size=10, hjust = 0.5, family="saira"),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

ggsave(p, filename = "2022/2022-01-25/all_games.jpg", width = 16, height = 16, unit = "in")

