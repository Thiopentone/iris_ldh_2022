################################################################################
# IRIS CONSORTIUM PROJECT                                                      #
# PUBLICATION II - LANCET DIGITAL HEALTH                                       #
# MASTER CODE                                                                  #
# DAVID SHAW, ANGELA BRUEGGEMANN                                               #                                                                   #
# UNIVERSITY OF OXFORD, NUFFIELD DEPARTMENT OF POPULATION HEALTH               #
# BRUEGGEMANN LABORATORY                                                       #
# 1ST EDITION: OCT 2021                                                        #
# CODE UPDATED: DEC 2022                                                       #
#                                                                              #
# DATA SETS UPDATED: 17/10/2022                                                #
# DATA SETS OBTAINED HERE: https://pubmlst.org/projects/iris                   #
# CONTACT: iris@ox.ac.uk                                                       #
# GitHub ID: Thiopentone                                                       #
################################################################################
# !NOTICE! 
# Due to data protection agreements, original data sets will NOT be made
# available to the public.
################################################################################

# PART A: SETTING UP
## Load packages (remove hash as required)

# install.packages("pacman")
# library(pacman)
pacman::p_load(readxl, tidyverse, lubridate, DataExplorer, GGally, explore, tableone)

## Set working directory (if required, otherwise continue in R project)

getwd()
# setwd("path/to/desired/folder")

## Call functions

source("Functions/summary_function.R")
source("Functions/summary_function_iso_yr.R")
source("Functions/months_function.R")

## Load data sets

# NOTE: data sets NOT provided
s_pneumoniae<-read_csv("Data Sets/s_pneumoniae.csv")
h_influenzae<-read_csv("Data Sets/h_influenzae.csv")
n_meningitidis<-read_csv("Data Sets/n_meningitidis.csv")
s_agalactiae<-read_csv("Data Sets/gbs.csv")

## Format data sets (repeat for each data set)

s_pneumoniae$date_sampled<-as.Date(s_pneumoniae$date_sampled, "%Y-%m-%d")
isoyear(s_pneumoniae$date_sampled)
isoweek(s_pneumoniae$date_sampled)

s_pneumoniae<- s_pneumoniae %>%
  mutate(age_range = recode(age_range,
                            "0-4" = "0-4",
                            "5-9" = "5-24",
                            "10-14" = "5-24",
                            "15-19" = "5-24",
                            "20-24" = "5-24",
                            "25-29" = "25-64",
                            "30-34" = "25-64",
                            "35-39" = "25-64",
                            "40-44" = "25-64",
                            "45-49" = "25-64",
                            "50-54" = "25-64",
                            "55-59" = "25-64",
                            "60-64" = "25-64",
                            "65-69" = "65+",
                            "70-74" = "65+",
                            "75-79" = "65+",
                            "80-84" = "65+",
                            "85-89" = "65+",
                            "90-94" = "65+",
                            "95-99" = "65+",
                            ">=100" = "65+"))
s_pneumoniae$age_range<- ordered(s_pneumoniae$age_range, levels = c("0-4", "5-24", "25-64", "65+"))

s_pneumoniae$country[s_pneumoniae$country=="China [Hong Kong]"]<-"China (Hong Kong, SAR)"
s_pneumoniae$country[s_pneumoniae$country=="UK [England]"]<-"England"
s_pneumoniae$country[s_pneumoniae$country=="UK [Northern Ireland]"]<-"Northern Ireland"
s_pneumoniae$country[s_pneumoniae$country=="UK [Scotland]"]<-"Scotland"
s_pneumoniae$country[s_pneumoniae$country=="UK [Wales]"]<-"Wales"

h_influenzae$date_sampled<-as.Date(h_influenzae$date_sampled, "%Y-%m-%d")
isoyear(h_influenzae$date_sampled)
isoweek(h_influenzae$date_sampled)

h_influenzae<- h_influenzae %>%
  mutate(age_range = recode(age_range,
                            "0-4" = "0-4",
                            "5-9" = "5-24",
                            "10-14" = "5-24",
                            "15-19" = "5-24",
                            "20-24" = "5-24",
                            "25-29" = "25-64",
                            "30-34" = "25-64",
                            "35-39" = "25-64",
                            "40-44" = "25-64",
                            "45-49" = "25-64",
                            "50-54" = "25-64",
                            "55-59" = "25-64",
                            "60-64" = "25-64",
                            "65-69" = "65+",
                            "70-74" = "65+",
                            "75-79" = "65+",
                            "80-84" = "65+",
                            "85-89" = "65+",
                            "90-94" = "65+",
                            "95-99" = "65+",
                            ">=100" = "65+"))
h_influenzae$age_range<- ordered(h_influenzae$age_range, levels = c("0-4", "5-24", "25-64", "65+"))

h_influenzae$country[h_influenzae$country=="China [Hong Kong]"]<-"China (Hong Kong, SAR)"
h_influenzae$country[h_influenzae$country=="UK [England]"]<-"England"
h_influenzae$country[h_influenzae$country=="UK [Northern Ireland]"]<-"Northern Ireland"
h_influenzae$country[h_influenzae$country=="UK [Scotland]"]<-"Scotland"
h_influenzae$country[h_influenzae$country=="UK [Wales]"]<-"Wales"

n_meningitidis$date_sampled<-as.Date(n_meningitidis$date_sampled, "%Y-%m-%d")
isoyear(n_meningitidis$date_sampled)
isoweek(n_meningitidis$date_sampled)
n_meningitidis<- n_meningitidis %>%
  mutate(age_range = recode(age_range,
                            "<1" = "0-4",
                            "1-4" = "0-4"))
n_meningitidis$age_range<- ordered(n_meningitidis$age_range, levels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+"))

n_meningitidis$age_range<- as.character(n_meningitidis$age_range)

n_meningitidis$age_range[n_meningitidis$age_range=="5-14"]<- "5-24"
n_meningitidis$age_range[n_meningitidis$age_range=="15-24"]<- "5-24"
n_meningitidis$age_range[n_meningitidis$age_range=="25-44"]<- "25-64"
n_meningitidis$age_range[n_meningitidis$age_range=="45-64"]<- "25-64"

n_meningitidis$age_range<- ordered(n_meningitidis$age_range, levels = c("0-4", "5-24", "25-64", "65+"))

n_meningitidis$country[n_meningitidis$country=="China [Hong Kong]"]<-"China (Hong Kong, SAR)"
n_meningitidis$country[n_meningitidis$country=="UK [England]"]<-"England"
n_meningitidis$country[n_meningitidis$country=="UK [Northern Ireland]"]<-"Northern Ireland"
n_meningitidis$country[n_meningitidis$country=="UK [Scotland]"]<-"Scotland"
n_meningitidis$country[n_meningitidis$country=="UK [Wales]"]<-"Wales"

## Generate a typical data frame (after cleaning and wrangling)

example<-data.frame(matrix(ncol=14, nrow = 5))
colnames(example)<-c("id", "country", "date_sampled", "date_received", "date_entered", "isoyear_sampled", "week_sampled",
                     "serotype", "age_yr", "age_months", "age_days", "age_range", "source", "diagnosis")
  
## Run exploratory data analyses (repeat for each organism)

### Generate report
s_pneumoniae %>%
  create_report(
    output_file = paste("EDA - ", deparse(substitute(s_pneumoniae))),
    report_title = paste("EDA Report -", deparse(substitute(s_pneumoniae))))

### Interactive data exploration
explore(s_pneumoniae)

# PART B: DESCRIPTIVE STATISTICS

monthly_median_iqr(s_pneumoniae) # calendar year
monthly_median_iqr_iso_yr(s_pneumoniae) # iso year

## Pre-pandemic vs pandemic
### Pre-pandemic = 0, pandemic = 1
s_pneumoniae$pan<-NA
s_pneumoniae$pan[s_pneumoniae$date_sampled<as.Date("2020-03-11")]<-0
s_pneumoniae$pan[s_pneumoniae$date_sampled>=as.Date("2020-03-11")]<-1

### Alternative stratification
s_pneumoniae$pan2<-NA
s_pneumoniae$pan2[s_pneumoniae$date_sampled<=as.Date("2019-12-31")]<-0
s_pneumoniae$pan2[s_pneumoniae$date_sampled>=as.Date("2020-01-01")]<-1
t1_sp<-as.data.frame(s_pneumoniae)

factorVars<- c("age_range", "sex", "continent", "serotype")
vars<- c("age_range", "sex", "continent", "serotype")
tableOne <- CreateTableOne(vars = vars, strata = "pan", data = t1_sp, factorVars = factorVars, includeNA = T, addOverall = T)
tableOne

tab3Mat <- print(tableOne, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab3Mat, "tabsy.csv")

################################################################################

# PART C.1: ARIMA MODELLING (REPEAT FOR EACH ORGANISM)

## Call "forecast" package by Rob Hyndman

pacman::p_load(forecast)

## Divide countries into hemispheres

southern<-c("Australia", "Brazil", "Colombia", "New Zealand", "Paraguay", "South Africa")
`%notin%`<-Negate(`%in%`)

# S PNEUMONIAE #
s_pneumoniae$hemisphere<-NA
s_pneumoniae$hemisphere[s_pneumoniae$country %notin% southern]<-"northern"
s_pneumoniae$hemisphere[s_pneumoniae$country %in% southern]<-"southern"

## Obtain case totals for each hemisphere (by calendar year)

sums<- s_pneumoniae %>%
  filter(date_sampled<=as.Date("2021-12-31")) %>%
  group_by(hemisphere) %>%
  summarise(count = n())
nh<- format(as.numeric(sums[1,2]), big.mark = ",")
sh<- format(as.numeric(sums[2,2]), big.mark = ",")

## Format dates and filter data set (restrict to cases occurring between 2018-2021)

s_pneumoniae$month_year<- format(as.Date(s_pneumoniae$date_sampled), "%b-%y")
s_pneumoniae_filtered<- s_pneumoniae %>% filter(date_sampled<=as.Date("2021-12-31"))

## Prepare "results" table

results<-data.frame(org=NA, hemisphere=NA, N=NA, cat=NA, value=NA, lower=NA, upper=NA)

## Prepare data for modelling (call "months_prep" function)

### Northern Hemisphere

months<-months_prep(s_pneumoniae_filtered, "northern", 27)

### Convert to time series data, explore, visualise, run augmented Dickey-Fuller test

ts<- ts(months$count, start = c(2018,01), frequency = 12)
ts
plot.ts(ts)
decompose.ts<- decompose(ts)
plot(decompose.ts)
ndiffs(ts) # number of differences to achieve stationarity
nsdiffs(ts) # number of seasonal differences to achieve stationarity
tseries::adf.test(ts) # unit root test
tseries::adf.test(diff(ts, 1)) # test with differencing if necessary

### Split time series before and after interventions

t<-27 # month when containment measures were put in place
months_before<- months[-c(t:length(months$month_year)), ]
months_after<- months[-c(1:(t-1)), ]
ts_before<- ts(months_before$count, start = c(2018,01), frequency = 12)
ts_after<- ts(months_after$count, start = c(2020,03), frequency = 12)

### ARIMA models without regressors (best fit using Hyndman-Khandakar algorithm) [Modelling the counterfactual]

model1<- auto.arima(ts, seasonal = TRUE, stepwise=F, trace=TRUE)
summary(model1)
#### Model diagnostics
checkresiduals(model1)

#### Choose P,D,Q and p,d,q values from model results above
model2 <- Arima(window(ts, end=c(2020,02)), order=c(2,0,0), seasonal=list(order=c(0,1,0), period=12))
fc <- forecast(model2, h=24)
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts)
ts.2

### ARIMA models with regressors

step<- months$step
slope<- months$slope

## Model with regressors [Assessing impact of containment measures / intervention]

##### Original model for whole series:
reg_1 <- Arima(ts, order=c(2,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_1)
coef(reg_1)
confint(reg_1)

#### Using best-fit approaches:
reg_2<- auto.arima(ts, seasonal = TRUE, xreg=cbind(step,slope), stepwise=F, trace=TRUE)
checkresiduals(reg_2)
coef(reg_2)
confint(reg_2)


#### Using time series before intervention:
reg_3 <- Arima(ts, order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_3)
coef(reg_3)
confint(reg_3)

### Forecasting the counterfactual

fc.ts2 <- ts(as.numeric(fc$lower[, '95%']), start=c(2020,03), frequency=12)
fc.ts3 <- ts(as.numeric(fc$upper[,'95%']), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts, fc.ts2, fc.ts3)
ts.2

ts.2<-data.frame(ts.2)
ts.2<- ts.2[-c(49:51), ]
ts.2$month<- data.frame(months$month_year)[1:48, ]
ts.2<- ts.2[, c(5, 1:4)]

### Relative risk of disease
pandemic<- ts.2[c(27:48), ]
rr<-(sum(pandemic$ts)/sum(pandemic$fc.ts))
upper<-(sum(pandemic$ts)/sum(pandemic$fc.ts2))
lower<-(sum(pandemic$ts)/sum(pandemic$fc.ts3))

### Cases averted
averted<-(sum(pandemic$fc.ts)-sum(pandemic$ts))
cases_lower<-(sum(pandemic$fc.ts2)-sum(pandemic$ts))
cases_upper<-(sum(pandemic$fc.ts3)-sum(pandemic$ts))

hem<-"northern"
org<-"s_pneumoniae"
results<-rbind(results, c(org, hem, nh, "rr", rr, lower, upper))
results<-rbind(results, c(org, hem, nh, "averted", averted, cases_lower, cases_upper))

### Southern Hemisphere

months<-months_prep(s_pneumoniae_filtered, "southern", 27)

### Convert to time series data, explore, visualise, run augmented Dickey-Fuller test

ts<- ts(months$count, start = c(2018,01), frequency = 12)
ts
plot.ts(ts)
decompose.ts<- decompose(ts)
plot(decompose.ts)
ndiffs(ts) # number of differences to achieve stationarity
nsdiffs(ts) # number of seasonal differences to achieve stationarity
tseries::adf.test(ts) # unit root test
tseries::adf.test(diff(ts, 1)) # test with differencing if necessary

### Split time series before and after interventions

t<-27
months_before<- months[-c(t:length(months$month_year)), ]
months_after<- months[-c(1:(t-1)), ]
ts_before<- ts(months_before$count, start = c(2018,01), frequency = 12)
ts_after<- ts(months_after$count, start = c(2020,03), frequency = 12)

### ARIMA models without regressors (best fit using Hyndman-Khandakar algorithm) [Modelling the counterfactual]

model1<- auto.arima(ts, seasonal = TRUE, stepwise=F, trace=TRUE)
summary(model1)
#### Model diagnostics
checkresiduals(model1)

#### Choose P,D,Q and p,d,q values from model results above
model2 <- Arima(window(ts, end=c(2020,02)), order=c(2,0,0), seasonal=list(order=c(0,1,0), period=12))
fc <- forecast(model2, h=24)
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts)
ts.2

### ARIMA models with regressors

step<- months$step
slope<- months$slope

## Model with regressors [Assessing impact of containment measures / intervention]

##### Original model for whole series:
reg_1 <- Arima(ts, order=c(2,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_1)
coef(reg_1)
confint(reg_1)

#### Using best-fit approaches:
reg_2<- auto.arima(ts, seasonal = TRUE, xreg=cbind(step,slope), stepwise=F, trace=TRUE)
checkresiduals(reg_2)
coef(reg_2)
confint(reg_2)


#### Using time series before intervention:
reg_3 <- Arima(ts, order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_3)
coef(reg_3)
confint(reg_3)

### Forecasting the counterfactual

fc.ts2 <- ts(as.numeric(fc$lower[, '95%']), start=c(2020,03), frequency=12)
fc.ts3 <- ts(as.numeric(fc$upper[,'95%']), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts, fc.ts2, fc.ts3)
ts.2

ts.2<-data.frame(ts.2)
ts.2<- ts.2[-c(49:51), ]
ts.2$month<- data.frame(months$month_year)[1:48, ]
ts.2<- ts.2[, c(5, 1:4)]

### Relative risk of disease
pandemic<- ts.2[c(27:48), ]
rr<-(sum(pandemic$ts)/sum(pandemic$fc.ts))
upper<-(sum(pandemic$ts)/sum(pandemic$fc.ts2))
lower<-(sum(pandemic$ts)/sum(pandemic$fc.ts3))

### Cases averted
averted<-(sum(pandemic$fc.ts)-sum(pandemic$ts))
cases_lower<-(sum(pandemic$fc.ts2)-sum(pandemic$ts))
cases_upper<-(sum(pandemic$fc.ts3)-sum(pandemic$ts))

hem<-"southern"
org<-"s_pneumoniae"
results<-rbind(results, c(org, hem, sh, "rr", rr, lower, upper))
results<-rbind(results, c(org, hem, sh, "averted", averted, cases_lower, cases_upper))

################################################################################
# H INFLUENZAE #
## Divide countries into hemispheres

h_influenzae$hemisphere<-NA
h_influenzae$hemisphere[h_influenzae$country %notin% southern]<-"northern"
h_influenzae$hemisphere[h_influenzae$country %in% southern]<-"southern"

sums<- h_influenzae %>%
  filter(date_sampled<=as.Date("2021-12-31")) %>%
  group_by(hemisphere) %>%
  summarise(count = n())
nh<- format(as.numeric(sums[1,2]), big.mark = ",")
sh<- format(as.numeric(sums[2,2]), big.mark = ",")

## Format dates and filter data set (restrict to cases occurring between 2018-2021)

h_influenzae$month_year<- format(as.Date(h_influenzae$date_sampled), "%b-%y")
h_influenzae_filtered<- h_influenzae %>% filter(date_sampled<=as.Date("2021-12-31"))

## Prepare data for modelling (call "months_prep" function)

### Northern Hemisphere

months<-months_prep(h_influenzae_filtered, "northern", 27)

### Convert to time series data, explore, visualise, run augmented Dickey-Fuller test

ts<- ts(months$count, start = c(2018,01), frequency = 12)
ts
plot.ts(ts)
decompose.ts<- decompose(ts)
plot(decompose.ts)
ndiffs(ts) # number of differences to achieve stationarity
nsdiffs(ts) # number of seasonal differences to achieve stationarity
tseries::adf.test(ts) # unit root test
tseries::adf.test(diff(ts, 1)) # test with differencing if necessary

### Split time series before and after interventions

t<-27
months_before<- months[-c(t:length(months$month_year)), ]
months_after<- months[-c(1:(t-1)), ]
ts_before<- ts(months_before$count, start = c(2018,01), frequency = 12)
ts_after<- ts(months_after$count, start = c(2020,03), frequency = 12)

### ARIMA models without regressors (best fit using Hyndman-Khandakar algorithm) [Modelling the counterfactual]

model1<- auto.arima(ts, seasonal = TRUE, stepwise=F, trace=TRUE)
summary(model1)
#### Model diagnostics
checkresiduals(model1)

#### Choose P,D,Q and p,d,q values from model results above
model2 <- Arima(window(ts, end=c(2020,02)), order=c(2,0,0), seasonal=list(order=c(0,1,0), period=12))
fc <- forecast(model2, h=24)
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts)
ts.2

### ARIMA models with regressors

step<- months$step
slope<- months$slope

## Model with regressors [Assessing impact of containment measures / intervention]

##### Original model for whole series:
reg_1 <- Arima(ts, order=c(2,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_1)
coef(reg_1)
confint(reg_1)

#### Using best-fit approaches:
reg_2<- auto.arima(ts, seasonal = TRUE, xreg=cbind(step,slope), stepwise=F, trace=TRUE)
checkresiduals(reg_2)
coef(reg_2)
confint(reg_2)


#### Using time series before intervention:
reg_3 <- Arima(ts, order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_3)
coef(reg_3)
confint(reg_3)

### Forecasting the counterfactual

fc.ts2 <- ts(as.numeric(fc$lower[, '95%']), start=c(2020,03), frequency=12)
fc.ts3 <- ts(as.numeric(fc$upper[,'95%']), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts, fc.ts2, fc.ts3)
ts.2

ts.2<-data.frame(ts.2)
ts.2<- ts.2[-c(49:51), ]
ts.2$month<- data.frame(months$month_year)[1:48, ]
ts.2<- ts.2[, c(5, 1:4)]

### Relative risk of disease
pandemic<- ts.2[c(27:48), ]
rr<-(sum(pandemic$ts)/sum(pandemic$fc.ts))
upper<-(sum(pandemic$ts)/sum(pandemic$fc.ts2))
lower<-(sum(pandemic$ts)/sum(pandemic$fc.ts3))

### Cases averted
averted<-(sum(pandemic$fc.ts)-sum(pandemic$ts))
cases_lower<-(sum(pandemic$fc.ts2)-sum(pandemic$ts))
cases_upper<-(sum(pandemic$fc.ts3)-sum(pandemic$ts))

hem<-"northern"
org<-"h_influenzae"
results<-rbind(results, c(org, hem, nh, "rr", rr, lower, upper))
results<-rbind(results, c(org, hem, nh, "averted", averted, cases_lower, cases_upper))

### Southern Hemisphere

months<-months_prep(h_influenzae_filtered, "southern", 27)

### Convert to time series data, explore, visualise, run augmented Dickey-Fuller test

ts<- ts(months$count, start = c(2018,01), frequency = 12)
ts
plot.ts(ts)
decompose.ts<- decompose(ts)
plot(decompose.ts)
ndiffs(ts) # number of differences to achieve stationarity
nsdiffs(ts) # number of seasonal differences to achieve stationarity
tseries::adf.test(ts) # unit root test
tseries::adf.test(diff(ts, 1)) # test with differencing if necessary

### Split time series before and after interventions

t<-27
months_before<- months[-c(t:length(months$month_year)), ]
months_after<- months[-c(1:(t-1)), ]
ts_before<- ts(months_before$count, start = c(2018,01), frequency = 12)
ts_after<- ts(months_after$count, start = c(2020,03), frequency = 12)

### ARIMA models without regressors (best fit using Hyndman-Khandakar algorithm) [Modelling the counterfactual]

model1<- auto.arima(ts, seasonal = TRUE, stepwise=F, trace=TRUE)
summary(model1)
#### Model diagnostics
checkresiduals(model1)

#### Choose P,D,Q and p,d,q values from model results above
model2 <- Arima(window(ts, end=c(2020,02)), order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12))
fc <- forecast(model2, h=24)
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts)
ts.2

### ARIMA models with regressors

step<- months$step
slope<- months$slope

## Model with regressors [Assessing impact of containment measures / intervention]

##### Original model for whole series:
reg_1 <- Arima(ts, order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_1)
coef(reg_1)
confint(reg_1)

#### Using best-fit approaches:
reg_2<- auto.arima(ts, seasonal = TRUE, xreg=cbind(step,slope), stepwise=F, trace=TRUE)
checkresiduals(reg_2)
coef(reg_2)
confint(reg_2)


#### Using time series before intervention:
reg_3 <- Arima(ts, order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_3)
coef(reg_3)
confint(reg_3)

### Forecasting the counterfactual

fc.ts2 <- ts(as.numeric(fc$lower[, '95%']), start=c(2020,03), frequency=12)
fc.ts3 <- ts(as.numeric(fc$upper[,'95%']), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts, fc.ts2, fc.ts3)
ts.2

ts.2<-data.frame(ts.2)
ts.2<- ts.2[-c(49:51), ]
ts.2$month<- data.frame(months$month_year)[1:48, ]
ts.2<- ts.2[, c(5, 1:4)]

### Relative risk of disease
pandemic<- ts.2[c(27:48), ]
rr<-(sum(pandemic$ts)/sum(pandemic$fc.ts))
upper<-(sum(pandemic$ts)/sum(pandemic$fc.ts2))
lower<-(sum(pandemic$ts)/sum(pandemic$fc.ts3))

### Cases averted
averted<-(sum(pandemic$fc.ts)-sum(pandemic$ts))
cases_lower<-(sum(pandemic$fc.ts2)-sum(pandemic$ts))
cases_upper<-(sum(pandemic$fc.ts3)-sum(pandemic$ts))

hem<-"southern"
org<-"h_influenzae"
results<-rbind(results, c(org, hem, sh, "rr", rr, lower, upper))
results<-rbind(results, c(org, hem, sh, "averted", averted, cases_lower, cases_upper))
################################################################################
# N MENINGITIDIS #
## Divide countries into hemispheres

n_meningitidis$hemisphere<-NA
n_meningitidis$hemisphere[n_meningitidis$country %notin% southern]<-"northern"
n_meningitidis$hemisphere[n_meningitidis$country %in% southern]<-"southern"

sums<- n_meningitidis %>%
  filter(date_sampled<=as.Date("2021-12-31")) %>%
  group_by(hemisphere) %>%
  summarise(count = n())
nh<- format(as.numeric(sums[1,2]), big.mark = ",")
sh<- format(as.numeric(sums[2,2]), big.mark = ",")

## Format dates and filter data set (restrict to cases occurring between 2018-2021)

n_meningitidis$month_year<- format(as.Date(n_meningitidis$date_sampled), "%b-%y")
n_meningitidis_filtered<- n_meningitidis %>% filter(date_sampled<=as.Date("2021-12-31"))

## Prepare data for modelling (call "months_prep" function)

### Northern Hemisphere

months<-months_prep(n_meningitidis_filtered, "northern", 27)

### Convert to time series data, explore, visualise, run augmented Dickey-Fuller test

ts<- ts(months$count, start = c(2018,01), frequency = 12)
ts
plot.ts(ts)
decompose.ts<- decompose(ts)
plot(decompose.ts)
ndiffs(ts) # number of differences to achieve stationarity
nsdiffs(ts) # number of seasonal differences to achieve stationarity
tseries::adf.test(ts) # unit root test
tseries::adf.test(diff(ts, 1)) # test with differencing if necessary

### Split time series before and after interventions

t<-27
months_before<- months[-c(t:length(months$month_year)), ]
months_after<- months[-c(1:(t-1)), ]
ts_before<- ts(months_before$count, start = c(2018,01), frequency = 12)
ts_after<- ts(months_after$count, start = c(2020,03), frequency = 12)

### ARIMA models without regressors (best fit using Hyndman-Khandakar algorithm) [Modelling the counterfactual]

model1<- auto.arima(ts, seasonal = TRUE, stepwise=F, trace=TRUE)
summary(model1)
#### Model diagnostics
checkresiduals(model1)

#### Choose P,D,Q and p,d,q values from model results above
model2 <- Arima(window(ts, end=c(2020,02)), order=c(1,0,0), seasonal=list(order=c(0,1,0), period=12))
fc <- forecast(model2, h=24)
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts)
ts.2

### ARIMA models with regressors

step<- months$step
slope<- months$slope

## Model with regressors [Assessing impact of containment measures / intervention]

##### Original model for whole series:
reg_1 <- Arima(ts, order=c(1,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_1)
coef(reg_1)
confint(reg_1)

#### Using best-fit approaches:
reg_2<- auto.arima(ts, seasonal = TRUE, xreg=cbind(step,slope), stepwise=F, trace=TRUE)
checkresiduals(reg_2)
coef(reg_2)
confint(reg_2)


#### Using time series before intervention:
reg_3 <- Arima(ts, order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_3)
coef(reg_3)
confint(reg_3)

### Forecasting the counterfactual

fc.ts2 <- ts(as.numeric(fc$lower[, '95%']), start=c(2020,03), frequency=12)
fc.ts3 <- ts(as.numeric(fc$upper[,'95%']), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts, fc.ts2, fc.ts3)
ts.2

ts.2<-data.frame(ts.2)
ts.2<- ts.2[-c(49:51), ]
ts.2$month<- data.frame(months$month_year)[1:48, ]
ts.2<- ts.2[, c(5, 1:4)]

### Relative risk of disease
pandemic<- ts.2[c(27:48), ]
rr<-(sum(pandemic$ts)/sum(pandemic$fc.ts))
upper<-(sum(pandemic$ts)/sum(pandemic$fc.ts2))
lower<-(sum(pandemic$ts)/sum(pandemic$fc.ts3))

### Cases averted
averted<-(sum(pandemic$fc.ts)-sum(pandemic$ts))
cases_lower<-(sum(pandemic$fc.ts2)-sum(pandemic$ts))
cases_upper<-(sum(pandemic$fc.ts3)-sum(pandemic$ts))

hem<-"northern"
org<-"n_meningitidis"
results<-rbind(results, c(org, hem, nh, "rr", rr, lower, upper))
results<-rbind(results, c(org, hem, nh, "averted", averted, cases_lower, cases_upper))

### Southern Hemisphere

months<-months_prep(n_meningitidis_filtered, "southern", 27)

### Convert to time series data, explore, visualise, run augmented Dickey-Fuller test

ts<- ts(months$count, start = c(2018,01), frequency = 12)
ts
plot.ts(ts)
decompose.ts<- decompose(ts)
plot(decompose.ts)
ndiffs(ts) # number of differences to achieve stationarity
nsdiffs(ts) # number of seasonal differences to achieve stationarity
tseries::adf.test(ts) # unit root test
tseries::adf.test(diff(ts, 1)) # test with differencing if necessary

### Split time series before and after interventions

t<-27
months_before<- months[-c(t:length(months$month_year)), ]
months_after<- months[-c(1:(t-1)), ]
ts_before<- ts(months_before$count, start = c(2018,01), frequency = 12)
ts_after<- ts(months_after$count, start = c(2020,03), frequency = 12)

### ARIMA models without regressors (best fit using Hyndman-Khandakar algorithm) [Modelling the counterfactual]

model1<- auto.arima(ts, seasonal = TRUE, stepwise=F, trace=TRUE)
summary(model1)
#### Model diagnostics
checkresiduals(model1)

#### Choose P,D,Q and p,d,q values from model results above
model2 <- Arima(window(ts, end=c(2020,02)), order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12))
fc <- forecast(model2, h=24)
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts)
ts.2

### ARIMA models with regressors

step<- months$step
slope<- months$slope

## Model with regressors [Assessing impact of containment measures / intervention]

##### Original model for whole series:
reg_1 <- Arima(ts, order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_1)
coef(reg_1)
confint(reg_1)

#### Using best-fit approaches:
reg_2<- auto.arima(ts, seasonal = TRUE, xreg=cbind(step,slope), stepwise=F, trace=TRUE)
checkresiduals(reg_2)
coef(reg_2)
confint(reg_2)


#### Using time series before intervention:
reg_3 <- Arima(ts, order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_3)
coef(reg_3)
confint(reg_3)

### Forecasting the counterfactual

fc.ts2 <- ts(as.numeric(fc$lower[, '95%']), start=c(2020,03), frequency=12)
fc.ts3 <- ts(as.numeric(fc$upper[,'95%']), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts, fc.ts2, fc.ts3)
ts.2

ts.2<-data.frame(ts.2)
ts.2<- ts.2[-c(49:51), ]
ts.2$month<- data.frame(months$month_year)[1:48, ]
ts.2<- ts.2[, c(5, 1:4)]

### Relative risk of disease
pandemic<- ts.2[c(27:48), ]
rr<-(sum(pandemic$ts)/sum(pandemic$fc.ts))
upper<-(sum(pandemic$ts)/sum(pandemic$fc.ts2))
lower<-(sum(pandemic$ts)/sum(pandemic$fc.ts3))

### Cases averted
averted<-(sum(pandemic$fc.ts)-sum(pandemic$ts))
cases_lower<-(sum(pandemic$fc.ts2)-sum(pandemic$ts))
cases_upper<-(sum(pandemic$fc.ts3)-sum(pandemic$ts))

hem<-"southern"
org<-"n_meningitidis"
results<-rbind(results, c(org, hem, sh, "rr", rr, lower, upper))
results<-rbind(results, c(org, hem, sh, "averted", averted, cases_lower, cases_upper))
################################################################################
# S AGALACTIAE #
## Divide countries into hemispheres

s_agalactiae$hemisphere<-NA
s_agalactiae$hemisphere[s_agalactiae$country %notin% southern]<-"northern"
s_agalactiae$hemisphere[s_agalactiae$country %in% southern]<-"southern"

sums<- s_agalactiae %>%
  filter(date_sampled<=as.Date("2021-12-31")) %>%
  group_by(hemisphere) %>%
  summarise(count = n())
nh<- format(as.numeric(sums[1,2]), big.mark = ",")

## Format dates and filter data set (restrict to cases occurring between 2018-2021)

s_agalactiae$month_year<- format(as.Date(s_agalactiae$date_sampled), "%b-%y")
s_agalactiae_filtered<- s_agalactiae %>% filter(date_sampled<=as.Date("2021-12-31"))

## Prepare data for modelling (call "months_prep" function)

### Northern Hemisphere

months<-months_prep(s_agalactiae_filtered, "northern", 27)

### Convert to time series data, explore, visualise, run augmented Dickey-Fuller test

ts<- ts(months$count, start = c(2018,01), frequency = 12)
ts
plot.ts(ts)
decompose.ts<- decompose(ts)
plot(decompose.ts)
ndiffs(ts) # number of differences to achieve stationarity
nsdiffs(ts) # number of seasonal differences to achieve stationarity
tseries::adf.test(ts) # unit root test
tseries::adf.test(diff(ts, 1)) # test with differencing if necessary

### Split time series before and after interventions

t<-27
months_before<- months[-c(t:length(months$month_year)), ]
months_after<- months[-c(1:(t-1)), ]
ts_before<- ts(months_before$count, start = c(2018,01), frequency = 12)
ts_after<- ts(months_after$count, start = c(2020,03), frequency = 12)

### ARIMA models without regressors (best fit using Hyndman-Khandakar algorithm) [Modelling the counterfactual]

model1<- auto.arima(ts, seasonal = TRUE, stepwise=F, trace=TRUE)
summary(model1)
#### Model diagnostics
checkresiduals(model1)

#### Choose P,D,Q and p,d,q values from model results above
model2 <- Arima(window(ts, end=c(2020,02)), order=c(0,1,1), seasonal=list(order=c(1,0,0), period=12))
fc <- forecast(model2, h=24)
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts)
ts.2

### ARIMA models with regressors

step<- months$step
slope<- months$slope

## Model with regressors [Assessing impact of containment measures / intervention]

##### Original model for whole series:
reg_1 <- Arima(ts, order=c(0,1,1), seasonal=list(order=c(1,0,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_1)
coef(reg_1)
confint(reg_1)

#### Using best-fit approaches:
reg_2<- auto.arima(ts, seasonal = TRUE, xreg=cbind(step,slope), stepwise=F, trace=TRUE)
checkresiduals(reg_2)
coef(reg_2)
confint(reg_2)


#### Using time series before intervention:
reg_3 <- Arima(ts, order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12),xreg=cbind(step,slope))
checkresiduals(reg_3)
coef(reg_3)
confint(reg_3)

### Forecasting the counterfactual

fc.ts2 <- ts(as.numeric(fc$lower[, '95%']), start=c(2020,03), frequency=12)
fc.ts3 <- ts(as.numeric(fc$upper[,'95%']), start=c(2020,03), frequency=12)
ts.2 <- ts.union(ts, fc.ts, fc.ts2, fc.ts3)
ts.2

ts.2<-data.frame(ts.2)
ts.2<- ts.2[-c(49:51), ]
ts.2$month<- data.frame(months$month_year)[1:48, ]
ts.2<- ts.2[, c(5, 1:4)]

### Relative risk of disease
pandemic<- ts.2[c(27:48), ]
rr<-(sum(pandemic$ts)/sum(pandemic$fc.ts))
upper<-(sum(pandemic$ts)/sum(pandemic$fc.ts2))
lower<-(sum(pandemic$ts)/sum(pandemic$fc.ts3))

### Cases averted
averted<-(sum(pandemic$fc.ts)-sum(pandemic$ts))
cases_lower<-(sum(pandemic$fc.ts2)-sum(pandemic$ts))
cases_upper<-(sum(pandemic$fc.ts3)-sum(pandemic$ts))

hem<-"northern"
org<-"s_agalactiae"
results<-rbind(results, c(org, hem, nh, "rr", rr, lower, upper))
results<-rbind(results, c(org, hem, nh, "averted", averted, cases_lower, cases_upper))
################################################################################
# PART C.2: META ANALYSIS OF RESULTS + CASES AVERTED
source("Functions/subgroup_meta_analysis.R")

## Cases averted
res_cases_averted<- results %>%
  filter(cat=="averted")
################################################################################
# PART D: SENSITIVITY ANALYSES

## Obtain population offsets

source("Functions/pop_offset_script.R")

## Load libraries

pacman::p_load(tsModel, MASS)

## Load segmented regression analysis function

source("Functions/seg_reg_sa.R")

## Run function

sp_sa<-seg_reg_sens_analysis(s_pneumoniae)
hi_sa<-seg_reg_sens_analysis(h_influenzae)
nm_sa<-seg_reg_sens_analysis(n_meningitidis)
gbs_sa<-seg_reg_sens_analysis(s_agalactiae)

################################################################################
# END                                                                          #
################################################################################
