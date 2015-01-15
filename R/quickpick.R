quickpick <-
function(enemy=c("","","","",""),teammate=c("","","","","")){
  winrate<-NULL
  antitable<-NULL
  matchtable<-NULL
  data(winrate, envir = environment())
  data(antitable, envir = environment())
  data(matchtable, envir=environment())
  enemy=enemy[enemy!=""]
  teammate=teammate[teammate!=""]
  col=2
  n=length(enemy)
  n2=length(teammate)
  if (n==0&n2==0){
    stop("Please type in at least one hero")
  }

  index=rep(0,time=n)
  index2=rep(0,time=n2)
  
#get hero index  
  if (n!=0){
    for (i in 1:n){
      temp=which(winrate[,1]==enemy[i])
      if (length(temp)==1){
        col=1
        index[i]=temp
        next
      }
      else
        temp=which(winrate[,3]==enemy[i])
      if (length(temp)==1){
        index[i]=temp
        next
      }
      else
        temp=which(winrate[,2]==enemy[i])
      
      if (length(temp)==0)
        stop("Please type in correct hero names in Chinese, Enlish or Korean.
\u53ef\u4ee5\u662f\u4e2d\u82f1\u6df7\u5408\uff0c\u4f46\u5747\u9700\u8981\u5168\u79f0\uff0c\u6ce8\u610f\u6807\u70b9\u5e94\u4e3a\u82f1\u6587")
      else
        index[i]=temp
    }
  }
  if (n2!=0){  
    for (i in 1:n2){
      temp=which(winrate[,1]==teammate[i])
      if (length(temp)==1){
        col=1
        index2[i]=temp
        next
      }
      else
        temp=which(winrate[,3]==teammate[i])
      if (length(temp)==1){
        index2[i]=temp
        next
      }
      else
        temp=which(winrate[,2]==teammate[i])
      if (length(temp)==0)
        stop("Please type in correct hero names in Chinese, Enlish or Korean.
\u53ef\u4ee5\u662f\u4e2d\u82f1\u6df7\u5408\uff0c\u4f46\u5747\u9700\u8981\u5168\u79f0\uff0c\u6ce8\u610f\u6807\u70b9\u5e94\u4e3a\u82f1\u6587")
      else
        index2[i]=temp
    }
  }
  win=data.frame(name=winrate[,col],winrate=rep(0,time=nrow(winrate)))
  win2=data.frame(name=winrate[,col],winrate=rep(0,time=nrow(winrate)))
  if (n!=0){subtable=winrate[index[1:n],c(1,4)]}
  if (n2!=0){subtable2=winrate[index2[1:n2],c(1,4)]}
  for (i in 1:nrow(winrate)){
    winr=winrate[i,4]
    loser=1-winrate[i,4]
    if (n!=0){
      for (j in 1:n){
        winr=winr*(1-subtable[j,2])
        loser=loser*subtable[j,2]}
    }
    if (n2!=0){
      for (j in 1:n2){
        winr=winr*subtable2[j,2]
        loser=loser*(1-subtable2[j,2])}
    }
    winr=winr/(winr+loser)
    if (n!=0){
      for (j in 1:n){
        winr=winr*(1+as.numeric(antitable[i,index[j]]))}
    }
    if (n2!=0){
      for (j in 1:n2){
        winr=winr*(1+as.numeric(matchtable[i,index2[j]]))}
    }
    win[i,2]=winr
    winr2=1
    if (n!=0){
      for (j in 1:n){
        winr2=winr2*(1+as.numeric(antitable[i,index[j]]))}
    }
    if (n2!=0){
      for (j in 1:n2){
        winr2=winr2*(1+as.numeric(matchtable[i,index2[j]]))}
    }
    win2[i,2]=winr2
  }
  win2=win2[-c(index,index2),]
  win=win[-c(index,index2),]

  names(win)=c("Names","Expected Winrate")
  names(win2)=c("Names","Expected Advantage")

  finalresult=cbind(head(win[order(win[,2],decreasing=T),]),head(win2[order(win2[,2],decreasing=T),]))
  print(finalresult)
}
