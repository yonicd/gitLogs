reshape_description=function(jsons,jsons.names){
  
  a=plyr::mdply(jsons,.fun = function(x) {
    data.frame(jsonlite::fromJSON(x)[[1]],stringsAsFactors = FALSE)},.progress = 'text')
  
  a$ON_CRAN=ifelse(a$Package%in%cran_current[,1],'CRAN_GITHUB','ONLY_GITHUB')
  
  a$repo=jsons.names[as.numeric(a$X1)]
  
  a1=a%>%dplyr::select(X1,ON_CRAN,repo,Package,Title,Author,Description,VignetteBuilder,BugReports,URL,Depends,Imports,Suggests,LinkingTo)%>%
    reshape2::melt(.,id= head(names(.),-4))%>%dplyr::filter(!is.na(value))
  
  # clean a bit more....
  a2=a1%>%plyr::ddply(head(names(a1),-1),.fun=function(x){
    data.frame(value=gsub(pattern = '^\\s+|\\s+$|\\s+\\((.*?)\\)|\\((.*?)\\)|\\b.1\\b|^s: ',
                          replacement = '',
                          x = strsplit(x$value,',')[[1]]
    ),
    stringsAsFactors = FALSE)
  },.progress = 'text')%>%dplyr::filter(!grepl(':|NULL',value))
  
  # reshape for rankings
  a3<-a2%>%plyr::dlply(.variables = c('ON_CRAN'),.fun=function(df){ 
    df%>%dplyr::count(variable,value)%>%dplyr::arrange(variable,desc(n))%>%
      dplyr::group_by(variable)%>%dplyr::do(.,cbind(rank=1:nrow(.),.))%>%
      dplyr::mutate(value=sprintf('%s (%s)',value,n))%>%
      reshape2::dcast(rank~variable,value.var='value')
  })
  l=list(raw=a,clean=a2,ranking=a3)
  
  return(l)
}