showwinrate <-
function(language="English"){
  winrate<-NULL
  data("winrate",envir = environment())
  if (language=="English")
    winrate[,c(2,4)]
  else if (language=="Korean")
    winrate[,c(3,4)]
  else if (language=="Chinese")
    winrate[,c(1,4)]
  else
    stop("Please type in correct language option('English','Korean' or 'Chinese')")
}
