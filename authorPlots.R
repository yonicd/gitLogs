gitAuthor=getAuthor(out)
bioconductor_current=getBioconductor()
bioAuthor=ddply(bioconductor_current,c('Package'),.fun=function(df){
  data.frame(Author=strsplit(df$Maintainer,',')[[1]],stringsAsFactors = FALSE)
})
cran_current=getPackagesWithTitle()


gitAuthor%>%count(Author)%>%arrange(desc(n))%>%
  top_n(20) %>%
  mutate(Author = reorder(Author, n)) %>%
  ggplot(aes(Author, n)) +
  geom_col(fill = "cyan4", alpha = 0.8,position = 'dodge',show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = "Number of Mentions in Author Field",
       title = "Who are the most common authors in CRAN and Github package descriptions?")




x<-gitAuthor%>%filter(Package%in%bioconductor_current$Package)%>%count(Author)%>%arrange(desc(n))%>%filter(n>=3)

Author_cors<-gitAuthor%>%filter(Author%in%x$Author)%>%
  mutate_each(funs(as.character))%>%ungroup%>%
  pairwise_cor(Author, Package, sort = TRUE)


filtered_cors <- Author_cors%>%
  filter(correlation > 0.15)

vertices <- x%>%ungroup%>%filter(Author %in% filtered_cors$item1)

set.seed(1234)
filtered_cors %>%
  graph_from_data_frame(vertices = vertices) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), width = 2) +
  geom_node_point(aes(size = n), color = "cyan4") +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  theme_graph() +
  scale_size_continuous(range = c(1, 15)) +
  labs(size = "Number of packages",
       edge_alpha = "Correlation",
       title = "Author Correlations of Bioconductor Packages %in% Github",
       subtitle = "Cliques of R package authors")
