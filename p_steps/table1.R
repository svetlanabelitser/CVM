# Program:      table1.R 
# Function:     table1
# Description:  create a table with counts and percentages for one categorical variable. Similar to 'tabyl'.
#
# Author:       Svetlana Belitser 


table1 <- function(x, title="", print=T, digits=2){
  x <- as.factor(x)
  if(print & title!="") cat(paste(title,"\n"))
  cbind( n=(tb<-table(x,useNA="ifany")), 
         percent=round(100*tb/sum(tb),digits), 
         percent2=c(round(100*(tb2<-table(x))/sum(tb2),digits),rep(NA,length(tb)-length(tb2))) )[,c(T,T,any(is.na(x)))]
}
#
#table1 <- function(x, digits=2){
#  x <- as.factor(x)
#  cbind( n=(tb<-table(x,useNA="ifany")), 
#         percent=round(100*tb/sum(tb),digits), 
#         percent2=c(round(100*(tb2<-table(x))/sum(tb2),digits),rep(NA,length(tb)-length(tb2))) )[,c(T,T,any(is.na(x)))]
#}

