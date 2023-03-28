## SUMMARY FUNCTION ISO YEAR ##

monthly_median_iqr_iso_yr<-function(x){
  summary<-data.frame("Year" = NA, "N" = NA, "Median" = NA, "IQR" = NA)
  
  years<- x %>%
    group_by(isoyear_sampled) %>%
    summarise(count = n())

  monthly<-x
  monthly$month<-format(x$date_sampled, "%b-%y")
  monthly$year<-format(x$date_sampled, "%Y")
  
  monthly_group<- monthly %>%
    filter(isoyear_sampled==2018) %>%
    group_by(month) %>%
    summarise(count = n())
  
  tot<-format(as.numeric(years %>% filter(isoyear_sampled==2018)%>%select(count)), big.mark = ",")
  med<-format(round(median(monthly_group$count), 0), big.mark = ",")
  iqr_l<-format(round(as.numeric(quantile(monthly_group$count, probs = (0.25))), 0), big.mark = ",")
  iqr_u<-format(round(as.numeric(quantile(monthly_group$count, probs = (0.75))), 0), big.mark = ",")
  
  iqr<-paste0(iqr_l, "-", iqr_u)
  
  summary<-rbind(summary, c(2018, tot, med, iqr))
  
  
  monthly_group<- monthly %>%
    filter(isoyear_sampled==2019) %>%
    group_by(month) %>%
    summarise(count = n())
  
  monthly_group<-monthly_group[order(monthly_group$month), ]
  monthly_group[6,2]<-monthly_group[6,2] + monthly_group[3,2]
  
  monthly_group<-monthly_group[-c(3), ]
  
  tot<-format(as.numeric(years %>% filter(isoyear_sampled==2019)%>%select(count)), big.mark = ",")
  med<-format(round(median(monthly_group$count), 0), big.mark = ",")
  iqr_l<-format(round(as.numeric(quantile(monthly_group$count, probs = (0.25))), 0), big.mark = ",")
  iqr_u<-format(round(as.numeric(quantile(monthly_group$count, probs = (0.75))), 0), big.mark = ",")
  
  iqr<-paste0(iqr_l, "-", iqr_u)
  
  summary<-rbind(summary, c(2019, tot, med, iqr))
  
  
  monthly_group<- monthly %>%
    filter(isoyear_sampled==2020) %>%
    group_by(month) %>%
    summarise(count = n())
  
  monthly_group<-monthly_group[order(monthly_group$month), ]
  monthly_group[6,2]<-monthly_group[6,2] + monthly_group[3,2]
  monthly_group[4,2]<-monthly_group[4,2] + monthly_group[7,2]
  
  monthly_group<-monthly_group[-c(3), ]
  monthly_group<-monthly_group[-c(7), ]
  
  tot<-format(as.numeric(years %>% filter(isoyear_sampled==2020)%>%select(count)), big.mark = ",")
  med<-format(round(median(monthly_group$count), 0), big.mark = ",")
  iqr_l<-format(round(as.numeric(quantile(monthly_group$count, probs = (0.25))), 0), big.mark = ",")
  iqr_u<-format(round(as.numeric(quantile(monthly_group$count, probs = (0.75))), 0), big.mark = ",")
  
  iqr<-paste0(iqr_l, "-", iqr_u)
  
  summary<-rbind(summary, c(2020, tot, med, iqr))
  
  
  monthly_group<- monthly %>%
    filter(isoyear_sampled==2021) %>%
    group_by(month) %>%
    summarise(count = n())
  
  monthly_group<-monthly_group[order(monthly_group$month), ]
  monthly_group[6,2]<-monthly_group[6,2] + monthly_group[3,2]
  
  monthly_group<-monthly_group[-c(3), ]
  
  tot<-format(as.numeric(years %>% filter(isoyear_sampled==2021)%>%select(count)), big.mark = ",")
  med<-format(round(median(monthly_group$count), 0), big.mark = ",")
  iqr_l<-format(round(as.numeric(quantile(monthly_group$count, probs = (0.25))), 0), big.mark = ",")
  iqr_u<-format(round(as.numeric(quantile(monthly_group$count, probs = (0.75))), 0), big.mark = ",")
  
  iqr<-paste0(iqr_l, "-", iqr_u)
  
  summary<-rbind(summary, c(2021, tot, med, iqr))
  summary<-summary[-c(1), ]
  return(summary)
}
  