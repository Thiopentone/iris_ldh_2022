## MONTHS FUNCTION ##

months_prep<-function(x,y,z){
months <- x %>%
  filter(hemisphere == y) %>%
  select(date_sampled, isoyear_sampled, month_year)

months <- months %>%
  group_by(month_year) %>%
  summarise(count = n())
months$month_year<-as.factor(months$month_year)
months$month_year<- ordered(months$month_year, levels = c("Jan-18", "Feb-18", "Mar-18", "Apr-18", "May-18", "Jun-18", "Jul-18", "Aug-18", "Sep-18", "Oct-18", "Nov-18", "Dec-18",
                                                          "Jan-19", "Feb-19", "Mar-19", "Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sep-19", "Oct-19", "Nov-19", "Dec-19",
                                                          "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "Dec-20",
                                                          "Jan-21", "Feb-21", "Mar-21", "Apr-21", "May-21", "Jun-21", "Jul-21", "Aug-21", "Sep-21", "Oct-21", "Nov-21", "Dec-21"))
months<-months[order(months$month_year), ]
months$month<- seq(1,length(months$month_year))

merger <- x %>%
  select(date_sampled, isoyear_sampled, month_year)

merger <- merger %>%
  group_by(month_year) %>%
  summarise(count = n())
merger$month_year<-as.factor(merger$month_year)
merger$month_year<- ordered(merger$month_year, levels = c("Jan-18", "Feb-18", "Mar-18", "Apr-18", "May-18", "Jun-18", "Jul-18", "Aug-18", "Sep-18", "Oct-18", "Nov-18", "Dec-18",
                                                          "Jan-19", "Feb-19", "Mar-19", "Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sep-19", "Oct-19", "Nov-19", "Dec-19",
                                                          "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "Dec-20",
                                                          "Jan-21", "Feb-21", "Mar-21", "Apr-21", "May-21", "Jun-21", "Jul-21", "Aug-21", "Sep-21", "Oct-21", "Nov-21", "Dec-21"))
merger<-merger[order(merger$month_year), ]
merger$month<- seq(1,length(merger$month_year))
merger<- merger[, -c(2,3)]

months<- merge(months, merger, all = T, by = "month_year")
months$count[is.na(months$count)]<- 0

months<-months[order(months$month_year), ]
months$month<- seq(1,length(months$month_year))

total<- sum(months$count)
max<- max(months$count)
min<- min(months$count)

t<- z

months$step<-0
months$step[months$month>=t]<-1

months$centered<- months$month-t
months$slope<- 0
months$slope<- months$centered+1
months$slope[months$month<t]<-0
months<- months[,-c(5) ]

step<- months$step
slope<- months$slope

return(months)
}