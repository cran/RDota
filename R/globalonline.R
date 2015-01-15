globalonline<-function(){
  url="http://dotamax.com/"
  doc=htmlTreeParse(url,useInternalNodes=T)
  what=xpathSApply(doc,"//div[@style='float: left;']",xmlValue)[[1]]
  as.numeric(what)
}