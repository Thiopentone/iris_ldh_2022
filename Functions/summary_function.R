## SUMMARY FUNCTION ##


monthly_median_iqr<-function(x){
  summary<-data.frame("Year" = NA, "N" = NA, "Median" = NA, "IQR" = NA)
  
#  years<- x %>%
#    group_by(isoyear_sampled) %>%
#    summarise(count = n())
  
  years<-x
  years$year<-format(years$date_sampled, "%Y")
  years<- years %>%
    group_by(year) %>%
    summarise(count = n())
  colnames(years)<-c("isoyear_sampled", "count")
  
  for(i in years$isoyear_sampled){
    monthly<-x
    monthly$month<-format(x$date_sampled, "%b-%y")
    monthly$year<-format(x$date_sampled, "%Y")
    
    monthly_group<- monthly %>%
      filter(year==i) %>%
      group_by(month) %>%
      summarise(count = n())
    tot<-format(as.numeric(years %>% filter(isoyear_sampled==i)%>%select(count)), big.mark = ",")
    med<-format(round(median(monthly_group$count), 0), big.mark = ",")
    iqr_l<-format(round(as.numeric(quantile(monthly_group$count, probs = (0.25))), 0), big.mark = ",")
    iqr_u<-format(round(as.numeric(quantile(monthly_group$count, probs = (0.75))), 0), big.mark = ",")
    
    iqr<-paste0(iqr_l, "-", iqr_u)
    
    summary<-rbind(summary, c(i, tot, med, iqr))
  }
  summary<-summary[-c(1), ]
  #summary<-t(summary)
  return(summary)
}
