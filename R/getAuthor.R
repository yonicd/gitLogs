#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param obj PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @importFrom plyr ldply
getAuthor=function(obj){

x<-sapply(obj$Author,function(x){
  s=gsub('<(.*?)>|\\[(.*?)\\]|\\((.*?)\\)',',',x)
  s=gsub('[\\"\\)\\&0-9;+]|\\n|\\band\\b|^$|\\bby\\b|\\bextending\\b',',',s)
  s=gsub(sprintf("[\\'.\\{\\}:\\/=@]| - |%s",paste0(sprintf("\\b%s\\b",authorTokens),collapse = '|')),'',s)
  
  s=gsub('á','a',s)
  s=gsub('Wickham Hadley','Hadley Wickham',s)
  s=gsub('Kent Russell|Russell Kent','Kenton Russell',s)
  s=gsub('Karl Broman','Karl W Broman',s)
  s=gsub('Rob Tibshirani','Robert Tibshirani',s)
  s=gsub('R Gentleman','Robert Gentleman',s)
  
  s=strsplit(s,',')[[1]]
  gsub('^\\s+|\\s+$','',s)
})

names(x)=obj$Package

plyr::ldply(x,function(y){
  data.frame(Author=unique(as.character(y[grepl(' ',y)])),stringsAsFactors = FALSE)
},.id='Package',.progress = 'text')%>%distinct()

}
