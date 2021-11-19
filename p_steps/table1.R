# Program:      table1.R 
# Function:     table1
# Description:  create a table with counts and percentages for one categorical variable. Similar to 'tabyl'.
#
# Author:       Svetlana Belitser 


table1 <- function(x, title="", digits=2, sep=" & ", print=T){
  if(length(dim(x))==2){
    for(icol in 2:ncol(x)) x[,1] <- paste(x[,1],x[,icol],sep=sep)
    x <- x[,1]
  }  
  else x <- as.factor(x)
  if(print & title!="") cat(paste(title,"\n"))
  cbind( n=(tb<-table(x,useNA="ifany")), 
         percent=round(100*tb/sum(tb),digits), 
         cum_percent=round(100*cumsum( tb/sum(tb) ),digits), 
         percent2=c(round(100*(tb2<-table(x))/sum(tb2),digits),rep(NA,length(tb)-length(tb2))),
         cum_percent2 = c(round(100*cumsum( (tb2<-table(x))/sum(tb2) ),digits),rep(NA,length(tb)-length(tb2))) 
         )[,c(T,T,T,any(is.na(x)),any(is.na(x)))]
}
#
#table1 <- function(x, digits=2){
#  x <- as.factor(x)
#  cbind( n=(tb<-table(x,useNA="ifany")), 
#         percent=round(100*tb/sum(tb),digits), 
#         percent2=c(round(100*(tb2<-table(x))/sum(tb2),digits),rep(NA,length(tb)-length(tb2))) )[,c(T,T,any(is.na(x)))]
#}

