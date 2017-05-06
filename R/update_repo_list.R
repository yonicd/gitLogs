update_repos_list<-function(){
  load('gitLogs/current_repo_list.Rdata')
  raw_json<-as.data.frame(do.call('cbind',jsonlite::fromJSON('http://rpkg.gepuro.net/download')$pkg_list),stringsAsFactors = FALSE)
  raw_json$pkg_name[which(!raw_json$pkg_name%in%JSONS.names)]
}