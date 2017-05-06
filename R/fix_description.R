#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param JSONS PARAM_DESCRIPTION
#' @param .progress PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @examples 
#' EXAMPLE1 
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
fix_description=function(JSONS,.progress=FALSE){
  tokens0=paste0(sprintf('^%s: ',tokens),collapse = '|')
  tokens1=paste0(sprintf('^%s:',tokens),collapse = '|')
  JSONS1=rep(NA,length(JSONS))
  if(.progress) pb <- utils::txtProgressBar(min=1,initial = 1,max = length(JSONS),style = 3)
  Ind=1:length(JSONS)
  for(ind in 1:length(JSONS)){
    if(.progress) utils::setTxtProgressBar(pb, ind)
    j=fromJSON(JSONS[ind])
    if(length(j[[1]])==0||ind%in%c(6616,7971,20438,24597)){
      JSONS1[ind]<-JSONS[ind]
    }else{
      fData=gsub(': NA$','',paste(names(j[[1]]),j[[1]],sep=': '))
      g0=grep("<<<<<<<",fData)
      if(length(g0)>0){
        g=grep("=======|>>>>>>>",fData)
        fData=fData[-c(g0,seq(g[1],g[2]))]
      }
      
      if(any(fData[!grepl(tokens0,fData)]%in%tokens)){
        m0=which(!grepl(tokens0,fData))
        m1=match(fData[!grepl(tokens0,fData)],tokens)
        m2=m1[!is.na(m1)]
        m3=tokens[m2]
        for(i in 1:length(m3)) {
          fData[m0[!is.na(m1)][i]]=gsub(m3[i],sprintf('%s:',m3[i]),fData[m0[!is.na(m1)][i]])
          }
      } 
      
      if(length(grep('^import:|^Import:',fData))==1) fData=gsub('import:|Import:','Imports:',fData)
      if(length(grep('^suggests|^Suggets:|^Suggest:',fData))==1) fData=gsub('^suggests|^Suggets:|^Suggest:','Suggests:',fData)
      if(length(grep('^Project|^IPackage|^liPackage',fData))==1) fData=gsub('^Project|IPackage|^liPackage','Package:',fData)
      if(length(grep('^Dependencies|^BuildDepends',fData))==1) fData=gsub('Dependencies|BuildDepends','Depends:',fData)
      if(length(grep('\r.[1-9]$',fData))>0) fData=gsub('\r.[1-9]','\r',fData)
      if(length(grep('\r[0-9]$',fData))>0) fData=gsub('\r[0-9]','\r',fData)
      if(length(grep('\\s+.[0-9]$',fData))>0) fData=fData[-grep('\\s+.[0-9]',fData)]
      if(length(grep('^[0-9]',fData))>0) fData=fData[-grep('^[0-9]',fData)]
      if(length(grep('^#',fData))>0) fData=fData[-grep('^#',fData)]
      if(length(grep('^GithubSHA1$',fData))>0) fData=fData[-grep('^GithubSHA1$',fData)]
      
      fData[which(!grepl(tokens1,fData))]=sprintf('  %s',fData[which(!grepl(tokens1,fData))])
      fData=fData[!grepl('^\\s+$',fData)]
          
      f=tempfile()
      cat(fData,file = f,sep = '\n')
      l<-list(as.list(as.data.frame(read.dcf(f),stringsAsFactors = FALSE)))
      names(l)=names(j)
      JSONS1[ind]<-toJSON(l)
      unlink(f)
    }
  }
  if(.progress) close(pb)
}