cran_current=getPackagesWithTitle()
bioconductor_current=getBioconductor()

cran <- dplyr::tbl_df(out$clean%>%dplyr::select(Package,Description)%>%dplyr::distinct())
cran$ON_CRAN=ifelse(cran$Package%in%cran_current[,1],'CRAN_GITHUB','ONLY_GITHUB')

tidy_cran <- cran %>%dplyr::group_by(ON_CRAN)%>%
  tidytext::unnest_tokens(word, Description)

word_totals <- tidy_cran %>%
  dplyr::anti_join(stop_words) %>%
  dplyr::count(word, sort = TRUE)

word_totals %>% 
  dplyr::group_by(ON_CRAN)%>%
  dplyr::top_n(20)%>%
  dplyr::arrange(ON_CRAN,desc(n))%>%
  dplyr::filter(!is.na(word))%>%
  dplyr::ungroup%>%
  dplyr::do(.,cbind(id=rep(stringr::str_pad(1:20,2,'left','0'),2),.))%>%
  dplyr::mutate(word1=paste(id,word,sep='_'))%>%
  ggplot2::ggplot(ggplot2::aes(word1, n)) +
  ggplot2::geom_col(ggplot2::aes(fill = ON_CRAN), alpha = 0.8,position = 'dodge',show.legend = FALSE) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(expand = c(0,0)) +
  ggplot2::labs(x = NULL, y = "Number of uses in CRAN descriptions",
       title = "What are the most commonly used words in CRAN and Github package descriptions?",
       subtitle = "After removing stop words")+facet_wrap(~ON_CRAN,scales = 'free_y')

netPlot <- plyr::dlply(tidy_cran%>%filter(!is.na(ON_CRAN)),c('ON_CRAN'),.fun=function(df){
  word_cors<-df%>%
    dplyr::anti_join(stop_words) %>%
    dplyr::group_by(word) %>%
    dplyr::filter(dplyr::n() > 150) %>% # filter for words used at least 150 times
    dplyr::ungroup %>%
    widyr::pairwise_cor(word, Package, sort = TRUE)
  
  
  filtered_cors <- word_cors%>%
    dplyr::filter(correlation > 0.2,
           item1 %in% word_totals$word,
           item2 %in% word_totals$word)
  
  vertices <- word_totals %>%
    dplyr::ungroup%>%
    dplyr::filter(ON_CRAN==unique(df$ON_CRAN))%>%
    dplyr::filter(word %in% filtered_cors$item1)%>%
    dplyr::select(-ON_CRAN)
  
  set.seed(1234)
  plotOut=filtered_cors %>%
    ggraph::graph_from_data_frame(vertices = vertices) %>%
    ggraph::ggraph(layout = "fr") +
    ggraph::geom_edge_link(ggraph::aes(edge_alpha = correlation), width = 2) +
    ggraph::geom_node_point(ggraph::aes(size = n), color = "cyan4") +
    ggraph::geom_node_text(ggraph::aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
    ggraph::theme_graph() +
    ggraph::scale_size_continuous(range = c(1, 15)) +
    ggraph::labs(size = "Number of uses",
         edge_alpha = "Correlation",
         title = sprintf("Word correlations in R package descriptions: %s",unique(df$ON_CRAN)),
         subtitle = "Which words are more likely to occur together than with other words?")

  list(plot=plotOut,word_cors=word_cors,filtered_cors=filtered_cors,vertices=vertices)
      
})

