library(tidyverse)
library(lubridate)
library(ggbump)
library(ggdist)
library(patchwork)
library(cowplot)
library(magick)

#read data
tuesdata <- tidytuesdayR::tt_load('2021-11-30')
matches <- tuesdata$matches

#plot bar chart
teams <- table(c(matches$team1, matches$team2))
teams_won <- table(matches$winner)
games_data <- tibble(team=names(teams), games=teams)
win_data <- tibble(team=names(teams_won), wins=teams_won)
plot_data1 <- left_join(games_data, win_data, by="team") %>%
  mutate(wins = replace_na(wins, 0), 
         losses = games - wins) %>%
  pivot_longer(cols=c("wins", "losses"), values_to = "num_games", names_to = "type") %>%
  mutate(teamf = fct_reorder(factor(team), games, min))

p1 <- ggplot(data=plot_data1, aes(x=teamf, y=num_games, fill=type)) + 
  geom_bar(stat = "identity", position="stack") +
  geom_text(data=filter(plot_data1, type=="losses"), 
            mapping=aes(x = teamf, y=games+10, label = teamf), 
            hjust = 0, colour = "#006629", family="serif") +
  coord_flip(expand = F) +
  scale_y_continuous(limits=c(0, 400)) +
  guides(fill=guide_legend(nrow=1, label.hjust=0)) +
  scale_fill_manual("", values=c("#006629", "#7ea881"), breaks=c("wins", "losses"), labels=c("Wins", "Losses")) +
  scale_colour_manual("", values=c("#006629", "#7ea881"), breaks=c("wins", "losses"), labels=c("Wins", "Losses")) +
  labs(x="", y="Number of games", title="ICC MEN'S CRICKET WORLD CUP\n") +
  theme(plot.background = element_rect(fill = "#D8E4D9", colour="#D8E4D9"),
        panel.background = element_rect(fill = "#D8E4D9", colour="#D8E4D9"),
        plot.title = element_text(colour = "#006629", size=22, face="bold", family="serif", hjust=0),
        plot.subtitle = element_text(colour = "#006629", size=12, family="serif"),
        plot.caption = element_text(colour = "#006629", size=12, family="serif", hjust=0),
        legend.background = element_rect(fill = "#D8E4D9", colour="#D8E4D9"),
        legend.key = element_rect(fill = "#D8E4D9", colour="#D8E4D9"), 
        legend.text =  element_text(colour = "#006629", size=12, family="serif"),
        legend.position=c(0.7, 0.2),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "#006629", size=12, family="serif"),
        axis.title = element_text(colour = "#006629", size=12, family="serif"),
        plot.margin = unit(c(0.3, 0.8, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1


#india vs pakistan
prep_data <- matches %>%
  filter(team1 %in% c("India", "Pakistan") & team2 %in% c("India", "Pakistan")) %>%
  select(match_date, team1, team2, score_team1, score_team2) %>%
  mutate(date = mdy(match_date))
pakistan <- rbind(filter(prep_data, team1 == "Pakistan") %>% select(date, score_team1) %>% rename(Pakistan = score_team1), 
                  filter(prep_data, team2 == "Pakistan") %>% select(date, score_team2) %>% rename(Pakistan = score_team2))
india <- rbind(filter(prep_data, team1 == "India") %>% select(date, score_team1) %>% rename(India = score_team1), 
               filter(prep_data, team2 == "India") %>% select(date, score_team2) %>% rename(India = score_team2))
plot_data2 <- left_join(india, pakistan, by="date") %>%
  pivot_longer(cols=c("India", "Pakistan"), names_to = "country", values_to = "score")

#geom bump of scores
p2 <- ggplot(plot_data2, aes(x=date, y=score, group=country, colour=country)) +
  geom_bump() +
  geom_point() + 
  guides(color=guide_legend(nrow=2, label.hjust=0.5)) +
  labs(x="", y="Score", 
       title="")  +
  scale_fill_manual("", values=c("#2255A4", "#006629")) +
  scale_colour_manual("", values=c("#2255A4", "#006629")) +
  theme(plot.background = element_rect(fill = "#D8E4D9", colour="#D8E4D9"),
        panel.background = element_rect(fill = "#D8E4D9", colour="#D8E4D9"),
        plot.title = element_text(colour = "#006629", size=18, face="bold", family="serif"),
        plot.subtitle = element_text(colour = "#006629", size=12, family="serif"),
        plot.caption = element_text(colour = "#006629", size=12, family="serif", hjust=0),
        legend.background = element_rect(fill = "#D8E4D9"),
        legend.key = element_rect(fill = "#D8E4D9", colour="#D8E4D9"), 
        legend.text =  element_text(colour = "#006629", size=12, family="serif"),
        legend.position=c(0.7, 0.2),
        axis.text = element_text(colour = "#006629", size=12, family="serif"),
        axis.title = element_text(colour = "#006629", size=12, family="serif"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2

#join plots together
p <- (p1 + plot_spacer()) / (plot_spacer() + p2) &
  theme(plot.background = element_rect(fill = "#D8E4D9", colour="#D8E4D9"),
        panel.background = element_rect(fill = "#D8E4D9", colour="#D8E4D9"))
p

#add and image
img <- image_read("bg.png")
q <- ggdraw() + 
  draw_plot(p) +
  draw_image(img, -0.2, -0.3, scale=0.4) +
  draw_label(x=0.75, y=0.73, hjust=0.5, 
             "The ICC Men's Cricket World Cup is the\ninternational championship of One Day\nInternational cricket held every four years.\nIndia and Pakistan dominate the Men's\nCricket World Cup, but what happens\nwhen they play each other?\n\nSince 1996, India and Pakistan\nhave faced each other 58 times\nwith Pakistan taking the win 35\ntimes - around 60%.", 
             color = "#006629", size = 12, fontfamily="serif") +
  draw_label(x=0.05, y=0.02, hjust=0, 
             "N. Rennie | Data: ESPN Cricinfo | Image: Wikipedia", 
             color = "#006629", size = 12, fontfamily="serif") 
q

