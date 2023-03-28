## RR CI ESTIMATION ##

# Estimate based on PI

ci_estimate<-function(x){
  df<-x
  df$lower<-as.numeric(df$lower)
  df$upper<-as.numeric(df$upper)
  df$value<-as.numeric(df$value)
df$a<-(log(df$lower)-log(df$value))/1.96
df$new<- exp(log(df$value)-1.96*df$a)
return(df)
}