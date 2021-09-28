library(tidyverse)
library(extrafont)
library(ggnetwork)
library(igraph)

#### read data ####
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

#### join data #### 
joined_df <- left_join(papers, paper_authors) %>% 
  left_join(authors) %>% 
  left_join(paper_programs) %>% 
  left_join(programs)%>% 
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    ),
    .after = paper
  ) 

#### prep graph data ####
#make adjacency matrix
hc_papers <- filter(joined_df, program == "HC")
hc_adj <- matrix(0, nrow=length(unique(hc_papers$author)), ncol=length(unique(hc_papers$author)))
#value equal to number of papers that authors have co-authored
for (i in 1:length(unique(hc_papers$author))){
  for (j in 1:length(unique(hc_papers$author))){
    if (i>j){
      #papers authored by these authors
      k <- filter(hc_papers, author == hc_papers$author[i] | author == hc_papers$author[j])
      #which are shared?
      num_authors <- table(k$paper)
      hc_adj[i,j] <- length(which(num_authors > 1))
      hc_adj[j,i] <- length(which(num_authors > 1))
    }
  }
}
#save files
saveRDS(hc_adj, "hc_adj.rds")
hc_adj <- readRDS("hc_adj.rds")

rownames(hc_adj) <- unique(hc_papers$author)
colnames(hc_adj) <- unique(hc_papers$author)
hc_adj_graph <- graph_from_adjacency_matrix(hc_adj, mode="undirected", weighted=T)
gnet <- ggnetwork(hc_adj_graph)

#size of bubble - number of papers
num_papers <- tibble(name=names(table(hc_papers$author)), vertex.size=as.numeric(table(hc_papers$author)))
gnet2 <- left_join(gnet, num_papers, by="name")

#### plot network ####
set.seed(280921)
pggnetwork <- 
  ggplot(data=gnet2, mapping=aes(x, y, xend = xend, yend = yend)) + 
  geom_edges(aes(size=weight), color = "#99AEAD", alpha=0.5) +
  geom_nodes(aes(size=vertex.size), colour="#6D9197") +
  theme_blank()+ 
  labs(title="\nNBER Health Care Authors\n",
       subtitle="Each node represents an author of a Health Care NBER paper, with the\n
       size of the node showing the number of papers they have written. Edges\n
       represent shared authorship, with the width of the edge representing the\n
       number of shared authorships. ",
       caption="N. Rennie | Data: NBER") +
  theme(panel.background = element_rect(fill = "#2F575D", colour="#2F575D"),
        plot.background = element_rect(fill = "#2F575D", colour="#2F575D"),
        plot.title = element_text(colour = "#99AEAD", size=20, face="bold", hjust = 0.5, family="Haettenschweiler"),
        plot.subtitle = element_text(colour = "#99AEAD", size=10, face="bold", hjust = 0.5, family="Segoe UI"),
        plot.caption = element_text(colour = "#99AEAD", size=10, face="bold", hjust = 0.5, family="Segoe UI"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
pggnetwork