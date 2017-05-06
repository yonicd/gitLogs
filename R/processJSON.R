new_repos=update_repos_list()

jsons=fetch_description(new_repos)
load('gitLogs/gitLogs.rdata')

parse_descriptions<-rep(NA,length(jsons))

for(i in 1:length(jsons)){
  x=jsons[[i]]
    if(!inherits(x,'raw')){
      if(!is.na(x)){
        f<-tempfile()
        cat(x,file = f)
        out=read.dcf(f)
        unlink(f)
        l=list(as.list(as.data.frame(out,stringsAsFactors = FALSE)))
        names(l)=names(jsons[i])
        parse_descriptions[i]<-toJSON(l)
    }
    }
  }

#cran_current=getPackagesWithTitle()
#bioconductor_current=getBioconductor()

JSONS=c(JSONS,parse_descriptions[!is.na(parse_descriptions)])

JSONS.names=sapply(JSONS,function(x) names(fromJSON(x)))
names(JSONS.names)<-NULL

out=reshape_description(JSONS,JSONS.names)

#print top 10
lapply(out$ranking,function(x) head(x,10))

save(JSONS,file='gitLogs/gitLogs.rdata')

save(JSONS.names,file='gitLogs/current_repo_list.Rdata')
