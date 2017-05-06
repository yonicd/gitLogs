fetch_description<-function(new_repos){
  pb <- utils::txtProgressBar(min = 1,max=length(new_repos),initial = 1,style=3)
  jsons<-sapply(1:length(new_repos),function(x){
    utils::setTxtProgressBar(pb, x)
    out=NULL
    out=httr::content(httr::GET(sprintf('https://raw.githubusercontent.com/%s/master/%s',new_repos[x],'DESCRIPTION')))
    ret=list(out)
    names(ret)=new_repos[x]
    ret
  })
  close(pb)
  return(jsons)
}