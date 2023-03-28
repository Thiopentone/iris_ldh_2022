## SEGMENTED REGRESSION SENSITIVITY ANALYSIS FUNCTION ##

seg_reg_sens_analysis<-function(dataframe){
  
  pacman::p_load(metafor, meta)
  

meta_quasipoisson<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
meta_quasipoisson_fourier<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
meta_nb<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
meta_nbf<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
meta_mod_qp_ext<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
meta_mod_qp_ext_f<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
meta_mod_nb_ext<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
meta_mod_nb_ext_f<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)

meta_averted<- data.frame("country" = 1, "averted" = 1, "lower" = 1, "upper" = 1, "model" = 1)
meta_averted_b<- data.frame("country" = 1, "averted" = 1, "lower" = 1, "upper" = 1, "model" = 1)
# Monthly
temp_data<- dataframe

dataframe<- dataframe %>%
  filter(country!="China"& country!="China (Hong Kong, SAR)" & country!= "South Korea" )

country<- dataframe %>%
  group_by(country) %>%
  summarise(count = n())

dataframe$month_year<- format(as.Date(dataframe$date_sampled), "%b-%y")
#dataframe<- dataframe %>% filter(isoyear_sampled!=2022)

for (i in country$country){
  
  months <- dataframe %>%
    filter(country == i) %>%
    dplyr::select(date_sampled, isoyear_sampled, month_year)
  
  months <- months %>%
    group_by(month_year) %>%
    summarise(count = n())
  months$month_year<-as.factor(months$month_year)
  months$month_year<- ordered(months$month_year, levels = c("Jan-18", "Feb-18", "Mar-18", "Apr-18", "May-18", "Jun-18", "Jul-18", "Aug-18", "Sep-18", "Oct-18", "Nov-18", "Dec-18",
                                                            "Jan-19", "Feb-19", "Mar-19", "Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sep-19", "Oct-19", "Nov-19", "Dec-19",
                                                            "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "Dec-20",
                                                            "Jan-21", "Feb-21", "Mar-21", "Apr-21", "May-21", "Jun-21", "Jul-21", "Aug-21", "Sep-21", "Oct-21", "Nov-21", "Dec-21",
                                                            "Jan-22", "Feb-22", "Mar-22"))
  months<-months[order(months$month_year), ]
  months$month<- seq(1,length(months$month_year))
  
  merger <- dataframe %>%
    dplyr::select(date_sampled, isoyear_sampled, month_year)
  
  merger <- merger %>%
    group_by(month_year) %>%
    summarise(count = n())
  merger$month_year<-as.factor(merger$month_year)
  merger$month_year<- ordered(merger$month_year, levels = c("Jan-18", "Feb-18", "Mar-18", "Apr-18", "May-18", "Jun-18", "Jul-18", "Aug-18", "Sep-18", "Oct-18", "Nov-18", "Dec-18",
                                                            "Jan-19", "Feb-19", "Mar-19", "Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sep-19", "Oct-19", "Nov-19", "Dec-19",
                                                            "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "Dec-20",
                                                            "Jan-21", "Feb-21", "Mar-21", "Apr-21", "May-21", "Jun-21", "Jul-21", "Aug-21", "Sep-21", "Oct-21", "Nov-21", "Dec-21",
                                                            "Jan-22", "Feb-22", "Mar-22"))
  merger<-merger[order(merger$month_year), ]
  merger$month<- seq(1,length(merger$month_year))
  merger<- merger[, -c(2,3)]
  
  months<- merge(months, merger, all = T, by = "month_year")
  months[is.na(months)]<- 0
  
  months<-months[order(months$month_year), ]
  months$month<- seq(1,length(months$month_year))
  
  months<-months[-c(49:length(months$month_year)), ]
  
  t<- 27
  
  months$step<-0
  months$step[months$month>=t]<-1
  
  named_months<- months$month_year
  months$named_months<-as.factor(substr(named_months, 1,3))
  
  population<- world %>%
    filter(country==i)
  x<- data.frame("country"=i, "year"=2021, "value"=population[3,3])
  population<- rbind(population, x)
  
  pop <- data.frame("pop" = as.numeric(rep(population[1,3], times = 12)))
  pop_2 <- data.frame("pop" = as.numeric(rep(population[2,3], times = 12)))
  pop_3 <- data.frame("pop" = as.numeric(rep(population[3,3], times = 12)))
  pop_4 <- data.frame("pop" = as.numeric(rep(population[4,3], times = 12)))
  
  pop<- rbind(pop, pop_2, pop_3, pop_4)
  months$pop<- log(pop$pop)
  
  counterfactual<- months
  counterfactual$step<- 0
  
  mod1<- glm(count~month + named_months + step + offset(pop), data = months, family = quasipoisson())
  model<- predict(mod1, type = "response")
  months<- cbind(months, model)
  
  model_cf<- predict(mod1, type = "response", newdata = counterfactual)
  months<- cbind(months, model_cf)
  
  fam<-family(mod1)
  ilink <- fam$linkinv
  ilink <- family(mod1)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(mod1, counterfactual, se.fit = TRUE)[1:2]),
                                       c('fit_link','se_link')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_link),
                   upr = ilink(fit_link + (1.96 * se_link)),
                   lwr = ilink(fit_link - (1.96 * se_link)))
  
  months$rr<- (months$model/months$model_cf)
  months$rr2<- (months$count/months$model_cf)
  
  ##
  
  months$upr[months$month<t]<-NA
  months$lwr[months$month<t]<-NA
  months$model_cf[months$month<t]<- NA
  
  meta_quasipoisson_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_cf)
  
  rr_upper<- sum(rr$count)/sum(rr$upr)
  rr_lower<- sum(rr$count)/sum(rr$lwr)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_cf)-sum(rr$count)
  averted_l<- sum(rr$lwr)-sum(rr$count)
  averted_h<- sum(rr$upr)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"QP"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_aa<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_quasipoisson_b[1,1]<- i
  meta_quasipoisson_b[1,2]<- rr_overall
  meta_quasipoisson_b[1,3]<- rr_upper
  meta_quasipoisson_b[1,4]<- rr_lower
  
  meta_quasipoisson<- rbind(meta_quasipoisson, meta_quasipoisson_b)
  
  modf<- glm(count~month + harmonic(month, 2, 12) + step + offset(pop), data = months, family = quasipoisson())
  modelf<- predict(modf, type = "response")
  months<- cbind(months, modelf)
  
  modelf_cf<- predict(modf, type = "response", newdata = counterfactual)
  months<- cbind(months, modelf_cf)
  
  fam<-family(modf)
  ilink <- fam$linkinv
  ilink <- family(modf)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(modf, counterfactual, se.fit = TRUE)[1:2]),
                                       c('fit_linkf','se_linkf')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_linkf),
                   uprf = ilink(fit_linkf + (1.96 * se_linkf)),
                   lwrf = ilink(fit_linkf - (1.96 * se_linkf)))
  
  months$rrf<- (months$modelf/months$modelf_cf)
  months$rrf2<- (months$count/months$modelf_cf)
  
  ##
  
  months$uprf[months$month<t]<-NA
  months$lwrf[months$month<t]<-NA
  months$modelf_cf[months$month<t]<- NA
  
  meta_quasipoisson_fourier_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$modelf_cf)
  
  rr_upper<- sum(rr$count)/sum(rr$uprf)
  rr_lower<- sum(rr$count)/sum(rr$lwrf)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$modelf_cf)-sum(rr$count)
  averted_l<- sum(rr$lwrf)-sum(rr$count)
  averted_h<- sum(rr$uprf)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"QP + F"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_quasipoisson_fourier_b[1,1]<- i
  meta_quasipoisson_fourier_b[1,2]<- rr_overall
  meta_quasipoisson_fourier_b[1,3]<- rr_upper
  meta_quasipoisson_fourier_b[1,4]<- rr_lower
  
  meta_quasipoisson_fourier<- rbind(meta_quasipoisson_fourier, meta_quasipoisson_fourier_b)
  
  pre<- months[c(1:t-1), ]
  
  mod_qp_ext<- glm(count~month + named_months + offset(pop), data = pre, family = quasipoisson())
  model_mod_qp_ext<- predict(mod_qp_ext, type = "response", newdata = months)
  months<- cbind(months, model_mod_qp_ext)
  
  fam<-family(mod_qp_ext)
  ilink <- fam$linkinv
  ilink <- family(mod_qp_ext)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(mod_qp_ext, months, se.fit = TRUE)[1:2]),
                                       c('fit_link_mod_qp_ext','se_link_mod_qp_ext')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_link_mod_qp_ext),
                   upr_mod_qp_ext = ilink(fit_link_mod_qp_ext + (1.96 * se_link_mod_qp_ext)),
                   lwr_mod_qp_ext = ilink(fit_link_mod_qp_ext - (1.96 * se_link_mod_qp_ext)))
  
  months$rr_mod_qp_ext<- (months$count/months$model_mod_qp_ext)
  
  ##
  
  months$upr_mod_qp_ext[months$month<t]<-NA
  months$lwr_mod_qp_ext[months$month<t]<-NA
  months$model_mod_qp_ext[months$month<t]<-NA
  
  meta_mod_qp_ext_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_mod_qp_ext)
  
  rr_upper<- sum(rr$count)/sum(rr$upr_mod_qp_ext)
  rr_lower<- sum(rr$count)/sum(rr$lwr_mod_qp_ext)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_mod_qp_ext)-sum(rr$count)
  averted_l<- sum(rr$lwr_mod_qp_ext)-sum(rr$count)
  averted_h<- sum(rr$upr_mod_qp_ext)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"QP Ext"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_mod_qp_ext_b[1,1]<- i
  meta_mod_qp_ext_b[1,2]<- rr_overall
  meta_mod_qp_ext_b[1,3]<- rr_upper
  meta_mod_qp_ext_b[1,4]<- rr_lower
  
  meta_mod_qp_ext<- rbind(meta_mod_qp_ext, meta_mod_qp_ext_b)
  #
  
  pre<- months[c(1:t-1), ]
  
  mod_qp_ext_f<- glm(count~month + harmonic(month, 2, 12) + offset(pop), data = pre, family = quasipoisson())
  model_mod_qp_ext_f<- predict(mod_qp_ext_f, type = "response", newdata = months)
  months<- cbind(months, model_mod_qp_ext_f)
  
  fam<-family(mod_qp_ext_f)
  ilink <- fam$linkinv
  ilink <- family(mod_qp_ext_f)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(mod_qp_ext_f, months, se.fit = TRUE)[1:2]),
                                       c('fit_link_mod_qp_ext_f','se_link_mod_qp_ext_f')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_link_mod_qp_ext_f),
                   upr_mod_qp_ext_f = ilink(fit_link_mod_qp_ext_f + (1.96 * se_link_mod_qp_ext_f)),
                   lwr_mod_qp_ext_f = ilink(fit_link_mod_qp_ext_f - (1.96 * se_link_mod_qp_ext_f)))
  
  months$rr_mod_qp_ext_f<- (months$count/months$model_mod_qp_ext_f)
  
  ##
  
  months$upr_mod_qp_ext_f[months$month<t]<-NA
  months$lwr_mod_qp_ext_f[months$month<t]<-NA
  months$model_mod_qp_ext_f[months$month<t]<-NA
  
  meta_mod_qp_ext_f_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_mod_qp_ext_f)
  
  rr_upper<- sum(rr$count)/sum(rr$upr_mod_qp_ext_f)
  rr_lower<- sum(rr$count)/sum(rr$lwr_mod_qp_ext_f)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_mod_qp_ext_f)-sum(rr$count)
  averted_l<- sum(rr$lwr_mod_qp_ext_f)-sum(rr$count)
  averted_h<- sum(rr$upr_mod_qp_ext_f)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"QP + F Ext"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_mod_qp_ext_f_b[1,1]<- i
  meta_mod_qp_ext_f_b[1,2]<- rr_overall
  meta_mod_qp_ext_f_b[1,3]<- rr_upper
  meta_mod_qp_ext_f_b[1,4]<- rr_lower
  
  meta_mod_qp_ext_f<- rbind(meta_mod_qp_ext_f, meta_mod_qp_ext_f_b)
  
  
  ## NB Model ##
  
  modnb<- glm.nb(count~month + named_months + step + offset(pop), data = months)
  modelnb<- predict(modnb, type = "response")
  months<- cbind(months, modelnb)
  
  model_cfnb<- predict(modnb, type = "response", newdata = counterfactual)
  months<- cbind(months, model_cfnb)
  
  fam<-family(modnb)
  ilink <- fam$linkinv
  ilink <- family(modnb)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(modnb, counterfactual, se.fit = TRUE)[1:2]),
                                       c('fit_linkn','se_linkn')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_linkn),
                   uprnb = ilink(fit_linkn + (1.96 * se_linkn)),
                   lwrnb = ilink(fit_linkn - (1.96 * se_linkn)))
  
  months$rrnb<- months$modelnb/months$model_cfnb
  months$rrnb2<- months$count/months$model_cfnb
  
  months$uprnb[months$month<t]<-NA
  months$lwrnb[months$month<t]<-NA
  months$model_cfnb[months$month<t]<- NA
  
  meta_nb_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_cf)
  
  rr_upper<- sum(rr$count)/sum(rr$uprnb)
  rr_lower<- sum(rr$count)/sum(rr$lwrnb)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_cfnb)-sum(rr$count)
  averted_l<- sum(rr$lwrnb)-sum(rr$count)
  averted_h<- sum(rr$uprnb)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"NB"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_nb_b[1,1]<- i
  meta_nb_b[1,2]<- rr_overall
  meta_nb_b[1,3]<- rr_upper
  meta_nb_b[1,4]<- rr_lower
  
  meta_nb<- rbind(meta_nb, meta_nb_b)
  
  # Fourier Terms
  modnbf<- glm.nb(count~month + harmonic(month, 2, 12) + step + offset(pop), data = months)
  modelnbf<- predict(modnbf, type = "response")
  months<- cbind(months, modelnbf)
  
  model_cfnbf<- predict(modnbf, type = "response", newdata = counterfactual)
  months<- cbind(months, model_cfnbf)
  
  fam<-family(modnbf)
  ilink <- fam$linkinv
  ilink <- family(modnbf)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(modnbf, counterfactual, se.fit = TRUE)[1:2]),
                                       c('fit_linknf','se_linknf')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_linknf),
                   uprnbf = ilink(fit_linknf + (1.96 * se_linknf)),
                   lwrnbf = ilink(fit_linknf - (1.96 * se_linknf)))
  
  months$rrnbf<- months$modelnbf/months$model_cfnbf
  months$rrnb2f<- months$count/months$model_cfnbf
  
  months$uprnbf[months$month<t]<-NA
  months$lwrnbf[months$month<t]<-NA
  months$model_cfnbf[months$month<t]<- NA
  
  meta_nbf_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_cfnbf)
  
  rr_upper<- sum(rr$count)/sum(rr$uprnbf)
  rr_lower<- sum(rr$count)/sum(rr$lwrnbf)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_cfnbf)-sum(rr$count)
  averted_l<- sum(rr$lwrnbf)-sum(rr$count)
  averted_h<- sum(rr$uprnbf)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"NB + F"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_nbf_b[1,1]<- i
  meta_nbf_b[1,2]<- rr_overall
  meta_nbf_b[1,3]<- rr_upper
  meta_nbf_b[1,4]<- rr_lower
  
  meta_nbf<- rbind(meta_nbf, meta_nbf_b)
  
  pre<- months[c(1:t-1), ]
  
  mod_nb_ext<- glm.nb(count~month + named_months + offset(pop), data = pre)
  model_mod_nb_ext<- predict(mod_nb_ext, type = "response", newdata = months)
  months<- cbind(months, model_mod_nb_ext)
  
  fam<-family(mod_nb_ext)
  ilink <- fam$linkinv
  ilink <- family(mod_nb_ext)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(mod_nb_ext, months, se.fit = TRUE)[1:2]),
                                       c('fit_link_mod_nb_ext','se_link_mod_nb_ext')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_link_mod_nb_ext),
                   upr_mod_nb_ext = ilink(fit_link_mod_nb_ext + (1.96 * se_link_mod_nb_ext)),
                   lwr_mod_nb_ext = ilink(fit_link_mod_nb_ext - (1.96 * se_link_mod_nb_ext)))
  
  months$rr_mod_nb_ext<- (months$count/months$model_mod_nb_ext)
  
  ##
  
  months$upr_mod_nb_ext[months$month<t]<-NA
  months$lwr_mod_nb_ext[months$month<t]<-NA
  months$model_mod_nb_ext[months$month<t]<-NA
  
  meta_mod_nb_ext_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_mod_nb_ext)
  
  rr_upper<- sum(rr$count)/sum(rr$upr_mod_nb_ext)
  rr_lower<- sum(rr$count)/sum(rr$lwr_mod_nb_ext)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_mod_nb_ext)-sum(rr$count)
  averted_l<- sum(rr$lwr_mod_nb_ext)-sum(rr$count)
  averted_h<- sum(rr$upr_mod_nb_ext)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"NB Ext"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_mod_nb_ext_b[1,1]<- i
  meta_mod_nb_ext_b[1,2]<- rr_overall
  meta_mod_nb_ext_b[1,3]<- rr_upper
  meta_mod_nb_ext_b[1,4]<- rr_lower
  
  meta_mod_nb_ext<- rbind(meta_mod_nb_ext, meta_mod_nb_ext_b)
  #
  
  pre<- months[c(1:t-1), ]
  
  mod_nb_ext_f<- glm.nb(count~month + harmonic(month, 2, 12) + offset(pop), data = pre)
  model_mod_nb_ext_f<- predict(mod_nb_ext_f, type = "response", newdata = months)
  months<- cbind(months, model_mod_nb_ext_f)
  
  fam<-family(mod_nb_ext_f)
  ilink <- fam$linkinv
  ilink <- family(mod_nb_ext_f)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(mod_nb_ext_f, months, se.fit = TRUE)[1:2]),
                                       c('fit_link_mod_nb_ext_f','se_link_mod_nb_ext_f')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_link_mod_nb_ext_f),
                   upr_mod_nb_ext_f = ilink(fit_link_mod_nb_ext_f + (1.96 * se_link_mod_nb_ext_f)),
                   lwr_mod_nb_ext_f = ilink(fit_link_mod_nb_ext_f - (1.96 * se_link_mod_nb_ext_f)))
  
  months$rr_mod_nb_ext_f<- (months$count/months$model_mod_nb_ext_f)
  
  ##
  
  months$upr_mod_nb_ext_f[months$month<t]<-NA
  months$lwr_mod_nb_ext_f[months$month<t]<-NA
  months$model_mod_nb_ext_f[months$month<t]<-NA
  
  meta_mod_nb_ext_f_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_mod_nb_ext_f)
  
  rr_upper<- sum(rr$count)/sum(rr$upr_mod_nb_ext_f)
  rr_lower<- sum(rr$count)/sum(rr$lwr_mod_nb_ext_f)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_mod_nb_ext_f)-sum(rr$count)
  averted_l<- sum(rr$lwr_mod_nb_ext_f)-sum(rr$count)
  averted_h<- sum(rr$upr_mod_nb_ext_f)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"NB + F Ext"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_mod_nb_ext_f_b[1,1]<- i
  meta_mod_nb_ext_f_b[1,2]<- rr_overall
  meta_mod_nb_ext_f_b[1,3]<- rr_upper
  meta_mod_nb_ext_f_b[1,4]<- rr_lower
  
  meta_mod_nb_ext_f<- rbind(meta_mod_nb_ext_f, meta_mod_nb_ext_f_b)
}
################################################################################
dataframe<- temp_data

dataframe<- dataframe %>%
  filter(country=="China" | country=="China (Hong Kong, SAR)" | country== "South Korea" )
country<- dataframe %>%
  group_by(country) %>%
  summarise(count = n())

dataframe$month_year<- format(as.Date(dataframe$date_sampled), "%b-%y")
#dataframe<- dataframe %>% filter(isoyear_sampled!=2022)

for (i in country$country){
  
  months <- dataframe %>%
    filter(country == i) %>%
    dplyr::select(date_sampled, isoyear_sampled, month_year)
  
  months <- months %>%
    group_by(month_year) %>%
    summarise(count = n())
  months$month_year<-as.factor(months$month_year)
  months$month_year<- ordered(months$month_year, levels = c("Jan-18", "Feb-18", "Mar-18", "Apr-18", "May-18", "Jun-18", "Jul-18", "Aug-18", "Sep-18", "Oct-18", "Nov-18", "Dec-18",
                                                            "Jan-19", "Feb-19", "Mar-19", "Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sep-19", "Oct-19", "Nov-19", "Dec-19",
                                                            "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "Dec-20",
                                                            "Jan-21", "Feb-21", "Mar-21", "Apr-21", "May-21", "Jun-21", "Jul-21", "Aug-21", "Sep-21", "Oct-21", "Nov-21", "Dec-21",
                                                            "Jan-22", "Feb-22", "Mar-22"))
  months<-months[order(months$month_year), ]
  months$month<- seq(1,length(months$month_year))
  
  merger <- dataframe %>%
    dplyr::select(date_sampled, isoyear_sampled, month_year)
  
  merger <- merger %>%
    group_by(month_year) %>%
    summarise(count = n())
  merger$month_year<-as.factor(merger$month_year)
  merger$month_year<- ordered(merger$month_year, levels = c("Jan-18", "Feb-18", "Mar-18", "Apr-18", "May-18", "Jun-18", "Jul-18", "Aug-18", "Sep-18", "Oct-18", "Nov-18", "Dec-18",
                                                            "Jan-19", "Feb-19", "Mar-19", "Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sep-19", "Oct-19", "Nov-19", "Dec-19",
                                                            "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "Dec-20",
                                                            "Jan-21", "Feb-21", "Mar-21", "Apr-21", "May-21", "Jun-21", "Jul-21", "Aug-21", "Sep-21", "Oct-21", "Nov-21", "Dec-21",
                                                            "Jan-22", "Feb-22", "Mar-22"))
  merger<-merger[order(merger$month_year), ]
  merger$month<- seq(1,length(merger$month_year))
  merger<- merger[, -c(2,3)]
  
  months<- merge(months, merger, all = T, by = "month_year")
  months[is.na(months)]<- 0
  
  months<-months[order(months$month_year), ]
  months$month<- seq(1,length(months$month_year))
  
  #months<-months[-c(49:length(months$month_year)), ]
  
  t<- 26
  
  months$step<-0
  months$step[months$month>=t]<-1
  
  named_months<- months$month_year
  months$named_months<-as.factor(substr(named_months, 1,3))
  
  population<- world %>%
    filter(country==i)
  x<- data.frame("country"=i, "year"=2021, "value"=population[3,3])
  population<- rbind(population, x)
  
  pop <- data.frame("pop" = as.numeric(rep(population[1,3], times = 12)))
  pop_2 <- data.frame("pop" = as.numeric(rep(population[2,3], times = 12)))
  pop_3 <- data.frame("pop" = as.numeric(rep(population[3,3], times = 12)))
  pop_4 <- data.frame("pop" = as.numeric(rep(population[4,3], times = 12)))
  
  pop<- rbind(pop, pop_2, pop_3, pop_4)
  months$pop<- log(pop$pop)
  
  counterfactual<- months
  counterfactual$step<- 0
  
  mod1<- glm(count~month + named_months + step + offset(pop), data = months, family = quasipoisson())
  model<- predict(mod1, type = "response")
  months<- cbind(months, model)
  
  model_cf<- predict(mod1, type = "response", newdata = counterfactual)
  months<- cbind(months, model_cf)
  
  fam<-family(mod1)
  ilink <- fam$linkinv
  ilink <- family(mod1)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(mod1, counterfactual, se.fit = TRUE)[1:2]),
                                       c('fit_link','se_link')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_link),
                   upr = ilink(fit_link + (1.96 * se_link)),
                   lwr = ilink(fit_link - (1.96 * se_link)))
  
  months$rr<- (months$model/months$model_cf)
  months$rr2<- (months$count/months$model_cf)
  
  ##
  
  months$upr[months$month<t]<-NA
  months$lwr[months$month<t]<-NA
  months$model_cf[months$month<t]<- NA
  
  meta_quasipoisson_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_cf)
  
  rr_upper<- sum(rr$count)/sum(rr$upr)
  rr_lower<- sum(rr$count)/sum(rr$lwr)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_cf)-sum(rr$count)
  averted_l<- sum(rr$lwr)-sum(rr$count)
  averted_h<- sum(rr$upr)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"QP"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_aa<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_quasipoisson_b[1,1]<- i
  meta_quasipoisson_b[1,2]<- rr_overall
  meta_quasipoisson_b[1,3]<- rr_upper
  meta_quasipoisson_b[1,4]<- rr_lower
  
  meta_quasipoisson<- rbind(meta_quasipoisson, meta_quasipoisson_b)
  
  modf<- glm(count~month + harmonic(month, 2, 12) + step + offset(pop), data = months, family = quasipoisson())
  modelf<- predict(modf, type = "response")
  months<- cbind(months, modelf)
  
  modelf_cf<- predict(modf, type = "response", newdata = counterfactual)
  months<- cbind(months, modelf_cf)
  
  fam<-family(modf)
  ilink <- fam$linkinv
  ilink <- family(modf)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(modf, counterfactual, se.fit = TRUE)[1:2]),
                                       c('fit_linkf','se_linkf')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_linkf),
                   uprf = ilink(fit_linkf + (1.96 * se_linkf)),
                   lwrf = ilink(fit_linkf - (1.96 * se_linkf)))
  
  months$rrf<- (months$modelf/months$modelf_cf)
  months$rrf2<- (months$count/months$modelf_cf)
  
  ##
  
  months$uprf[months$month<t]<-NA
  months$lwrf[months$month<t]<-NA
  months$modelf_cf[months$month<t]<- NA
  
  meta_quasipoisson_fourier_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$modelf_cf)
  
  rr_upper<- sum(rr$count)/sum(rr$uprf)
  rr_lower<- sum(rr$count)/sum(rr$lwrf)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$modelf_cf)-sum(rr$count)
  averted_l<- sum(rr$lwrf)-sum(rr$count)
  averted_h<- sum(rr$uprf)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"QP + F"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_quasipoisson_fourier_b[1,1]<- i
  meta_quasipoisson_fourier_b[1,2]<- rr_overall
  meta_quasipoisson_fourier_b[1,3]<- rr_upper
  meta_quasipoisson_fourier_b[1,4]<- rr_lower
  
  meta_quasipoisson_fourier<- rbind(meta_quasipoisson_fourier, meta_quasipoisson_fourier_b)
  
  pre<- months[c(1:t-1), ]
  
  mod_qp_ext<- glm(count~month + named_months + offset(pop), data = pre, family = quasipoisson())
  model_mod_qp_ext<- predict(mod_qp_ext, type = "response", newdata = months)
  months<- cbind(months, model_mod_qp_ext)
  
  fam<-family(mod_qp_ext)
  ilink <- fam$linkinv
  ilink <- family(mod_qp_ext)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(mod_qp_ext, months, se.fit = TRUE)[1:2]),
                                       c('fit_link_mod_qp_ext','se_link_mod_qp_ext')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_link_mod_qp_ext),
                   upr_mod_qp_ext = ilink(fit_link_mod_qp_ext + (1.96 * se_link_mod_qp_ext)),
                   lwr_mod_qp_ext = ilink(fit_link_mod_qp_ext - (1.96 * se_link_mod_qp_ext)))
  
  months$rr_mod_qp_ext<- (months$count/months$model_mod_qp_ext)
  
  ##
  
  months$upr_mod_qp_ext[months$month<t]<-NA
  months$lwr_mod_qp_ext[months$month<t]<-NA
  months$model_mod_qp_ext[months$month<t]<-NA
  
  meta_mod_qp_ext_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_mod_qp_ext)
  
  rr_upper<- sum(rr$count)/sum(rr$upr_mod_qp_ext)
  rr_lower<- sum(rr$count)/sum(rr$lwr_mod_qp_ext)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_mod_qp_ext)-sum(rr$count)
  averted_l<- sum(rr$lwr_mod_qp_ext)-sum(rr$count)
  averted_h<- sum(rr$upr_mod_qp_ext)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"QP Ext"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_mod_qp_ext_b[1,1]<- i
  meta_mod_qp_ext_b[1,2]<- rr_overall
  meta_mod_qp_ext_b[1,3]<- rr_upper
  meta_mod_qp_ext_b[1,4]<- rr_lower
  
  meta_mod_qp_ext<- rbind(meta_mod_qp_ext, meta_mod_qp_ext_b)
  
  pre<- months[c(1:t-1), ]
  
  mod_qp_ext_f<- glm(count~month + harmonic(month, 2, 12) + offset(pop), data = pre, family = quasipoisson())
  model_mod_qp_ext_f<- predict(mod_qp_ext_f, type = "response", newdata = months)
  months<- cbind(months, model_mod_qp_ext_f)
  
  fam<-family(mod_qp_ext_f)
  ilink <- fam$linkinv
  ilink <- family(mod_qp_ext_f)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(mod_qp_ext_f, months, se.fit = TRUE)[1:2]),
                                       c('fit_link_mod_qp_ext_f','se_link_mod_qp_ext_f')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_link_mod_qp_ext_f),
                   upr_mod_qp_ext_f = ilink(fit_link_mod_qp_ext_f + (1.96 * se_link_mod_qp_ext_f)),
                   lwr_mod_qp_ext_f = ilink(fit_link_mod_qp_ext_f - (1.96 * se_link_mod_qp_ext_f)))
  
  months$rr_mod_qp_ext_f<- (months$count/months$model_mod_qp_ext_f)
  
  ##
  
  months$upr_mod_qp_ext_f[months$month<t]<-NA
  months$lwr_mod_qp_ext_f[months$month<t]<-NA
  months$model_mod_qp_ext_f[months$month<t]<-NA
  
  meta_mod_qp_ext_f_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_mod_qp_ext_f)
  
  rr_upper<- sum(rr$count)/sum(rr$upr_mod_qp_ext_f)
  rr_lower<- sum(rr$count)/sum(rr$lwr_mod_qp_ext_f)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_mod_qp_ext_f)-sum(rr$count)
  averted_l<- sum(rr$lwr_mod_qp_ext_f)-sum(rr$count)
  averted_h<- sum(rr$upr_mod_qp_ext_f)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"QP + F Ext"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_mod_qp_ext_f_b[1,1]<- i
  meta_mod_qp_ext_f_b[1,2]<- rr_overall
  meta_mod_qp_ext_f_b[1,3]<- rr_upper
  meta_mod_qp_ext_f_b[1,4]<- rr_lower
  
  meta_mod_qp_ext_f<- rbind(meta_mod_qp_ext_f, meta_mod_qp_ext_f_b)
  
  
  ## NB Model ##
  
  modnb<- glm.nb(count~month + named_months + step + offset(pop), data = months)
  modelnb<- predict(modnb, type = "response")
  months<- cbind(months, modelnb)
  
  model_cfnb<- predict(modnb, type = "response", newdata = counterfactual)
  months<- cbind(months, model_cfnb)
  
  fam<-family(modnb)
  ilink <- fam$linkinv
  ilink <- family(modnb)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(modnb, counterfactual, se.fit = TRUE)[1:2]),
                                       c('fit_linkn','se_linkn')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_linkn),
                   uprnb = ilink(fit_linkn + (1.96 * se_linkn)),
                   lwrnb = ilink(fit_linkn - (1.96 * se_linkn)))
  
  months$rrnb<- months$modelnb/months$model_cfnb
  months$rrnb2<- months$count/months$model_cfnb
  
  months$uprnb[months$month<t]<-NA
  months$lwrnb[months$month<t]<-NA
  months$model_cfnb[months$month<t]<- NA
  
  meta_nb_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_cf)
  
  rr_upper<- sum(rr$count)/sum(rr$uprnb)
  rr_lower<- sum(rr$count)/sum(rr$lwrnb)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_cfnb)-sum(rr$count)
  averted_l<- sum(rr$lwrnb)-sum(rr$count)
  averted_h<- sum(rr$uprnb)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"NB"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_nb_b[1,1]<- i
  meta_nb_b[1,2]<- rr_overall
  meta_nb_b[1,3]<- rr_upper
  meta_nb_b[1,4]<- rr_lower
  
  meta_nb<- rbind(meta_nb, meta_nb_b)
  
  # Fourier Terms
  modnbf<- glm.nb(count~month + harmonic(month, 2, 12) + step + offset(pop), data = months)
  modelnbf<- predict(modnbf, type = "response")
  months<- cbind(months, modelnbf)
  
  model_cfnbf<- predict(modnbf, type = "response", newdata = counterfactual)
  months<- cbind(months, model_cfnbf)
  
  fam<-family(modnbf)
  ilink <- fam$linkinv
  ilink <- family(modnbf)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(modnbf, counterfactual, se.fit = TRUE)[1:2]),
                                       c('fit_linknf','se_linknf')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_linknf),
                   uprnbf = ilink(fit_linknf + (1.96 * se_linknf)),
                   lwrnbf = ilink(fit_linknf - (1.96 * se_linknf)))
  
  months$rrnbf<- months$modelnbf/months$model_cfnbf
  months$rrnb2f<- months$count/months$model_cfnbf
  
  months$uprnbf[months$month<t]<-NA
  months$lwrnbf[months$month<t]<-NA
  months$model_cfnbf[months$month<t]<- NA
  
  meta_nbf_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_cfnbf)
  
  rr_upper<- sum(rr$count)/sum(rr$uprnbf)
  rr_lower<- sum(rr$count)/sum(rr$lwrnbf)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_cfnbf)-sum(rr$count)
  averted_l<- sum(rr$lwrnbf)-sum(rr$count)
  averted_h<- sum(rr$uprnbf)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"NB + F"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_nbf_b[1,1]<- i
  meta_nbf_b[1,2]<- rr_overall
  meta_nbf_b[1,3]<- rr_upper
  meta_nbf_b[1,4]<- rr_lower
  
  meta_nbf<- rbind(meta_nbf, meta_nbf_b)
  ##
  
  pre<- months[c(1:t-1), ]
  
  mod_nb_ext<- glm.nb(count~month + named_months + offset(pop), data = pre)
  model_mod_nb_ext<- predict(mod_nb_ext, type = "response", newdata = months)
  months<- cbind(months, model_mod_nb_ext)
  
  fam<-family(mod_nb_ext)
  ilink <- fam$linkinv
  ilink <- family(mod_nb_ext)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(mod_nb_ext, months, se.fit = TRUE)[1:2]),
                                       c('fit_link_mod_nb_ext','se_link_mod_nb_ext')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_link_mod_nb_ext),
                   upr_mod_nb_ext = ilink(fit_link_mod_nb_ext + (1.96 * se_link_mod_nb_ext)),
                   lwr_mod_nb_ext = ilink(fit_link_mod_nb_ext - (1.96 * se_link_mod_nb_ext)))
  
  months$rr_mod_nb_ext<- (months$count/months$model_mod_nb_ext)
  
  ##
  
  months$upr_mod_nb_ext[months$month<t]<-NA
  months$lwr_mod_nb_ext[months$month<t]<-NA
  months$model_mod_nb_ext[months$month<t]<-NA
  
  meta_mod_nb_ext_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_mod_nb_ext)
  
  rr_upper<- sum(rr$count)/sum(rr$upr_mod_nb_ext)
  rr_lower<- sum(rr$count)/sum(rr$lwr_mod_nb_ext)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_mod_nb_ext)-sum(rr$count)
  averted_l<- sum(rr$lwr_mod_nb_ext)-sum(rr$count)
  averted_h<- sum(rr$upr_mod_nb_ext)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"NB Ext"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_mod_nb_ext_b[1,1]<- i
  meta_mod_nb_ext_b[1,2]<- rr_overall
  meta_mod_nb_ext_b[1,3]<- rr_upper
  meta_mod_nb_ext_b[1,4]<- rr_lower
  
  meta_mod_nb_ext<- rbind(meta_mod_nb_ext, meta_mod_nb_ext_b)
  #
  
  pre<- months[c(1:t-1), ]
  
  mod_nb_ext_f<- glm.nb(count~month + harmonic(month, 2, 12) + offset(pop), data = pre)
  model_mod_nb_ext_f<- predict(mod_nb_ext_f, type = "response", newdata = months)
  months<- cbind(months, model_mod_nb_ext_f)
  
  fam<-family(mod_nb_ext_f)
  ilink <- fam$linkinv
  ilink <- family(mod_nb_ext_f)$linkinv
  months <- bind_cols(months, setNames(as_tibble(predict(mod_nb_ext_f, months, se.fit = TRUE)[1:2]),
                                       c('fit_link_mod_nb_ext_f','se_link_mod_nb_ext_f')))  
  months <- mutate(months,
                   fit_resp  = ilink(fit_link_mod_nb_ext_f),
                   upr_mod_nb_ext_f = ilink(fit_link_mod_nb_ext_f + (1.96 * se_link_mod_nb_ext_f)),
                   lwr_mod_nb_ext_f = ilink(fit_link_mod_nb_ext_f - (1.96 * se_link_mod_nb_ext_f)))
  
  months$rr_mod_nb_ext_f<- (months$count/months$model_mod_nb_ext_f)
  
  ##
  
  months$upr_mod_nb_ext_f[months$month<t]<-NA
  months$lwr_mod_nb_ext_f[months$month<t]<-NA
  months$model_mod_nb_ext_f[months$month<t]<-NA
  
  meta_mod_nb_ext_f_b<- data.frame("country" = 1, "rr"=1, "lwr"=1, "upr"=1)
  
  rr<- months[c(t:length(months$month)), ]
  rr_overall<- sum(rr$count)/sum(rr$model_mod_nb_ext_f)
  
  rr_upper<- sum(rr$count)/sum(rr$upr_mod_nb_ext_f)
  rr_lower<- sum(rr$count)/sum(rr$lwr_mod_nb_ext_f)
  
  risk_ratio<- paste("RR = ", round(rr_overall, 2), " 95% CI: ", round(rr_upper, 2), "; ", round(rr_lower, 2), sep = "")
  
  averted<- sum(rr$model_mod_nb_ext_f)-sum(rr$count)
  averted_l<- sum(rr$lwr_mod_nb_ext_f)-sum(rr$count)
  averted_h<- sum(rr$upr_mod_nb_ext_f)-sum(rr$count)
  
  meta_averted_b[1,1]<-i
  meta_averted_b[1,2]<-averted
  meta_averted_b[1,3]<-averted_l
  meta_averted_b[1,4]<-averted_h
  meta_averted_b[1,5]<-"NB + F Ext"
  meta_averted<- rbind(meta_averted, meta_averted_b)
  
  averted_all<- paste("Averted = ", format(round(averted, 0), big.mark = ",")," cases, ", " 95% CI: ", format(round(averted_l, 0), big.mark = ","), "; ", format(round(averted_h, 0), big.mark = ","), sep = "")
  
  label_a<- paste(risk_ratio, averted_all, sep = "\n")
  
  meta_mod_nb_ext_f_b[1,1]<- i
  meta_mod_nb_ext_f_b[1,2]<- rr_overall
  meta_mod_nb_ext_f_b[1,3]<- rr_upper
  meta_mod_nb_ext_f_b[1,4]<- rr_lower
  
  meta_mod_nb_ext_f<- rbind(meta_mod_nb_ext_f, meta_mod_nb_ext_f_b)
}

model<- meta_averted %>%
  group_by(model) %>%
  summarise(count = n())
model<- model[-c(1), ]

results<- data.frame("averted"=1, "lower"=1, "upper"=1, "model"=1)

for (i in model$model){
  x<-meta_averted %>% 
    filter(model==i)
  x$upper[!is.finite(x$upper)] <- NA
  results_b<- data.frame("averted"=1, "lower"=1, "upper"=1, "model"=1)
  results_b[1,1]<-round(sum(x$averted, na.rm = T), 0)
  results_b[1,2]<-round(sum(x$lower, na.rm = T), 0)
  results_b[1,3]<-round(sum(x$upper, na.rm = T), 0)
  results_b[1,4]<-i
  results<- rbind(results, results_b)
}

results<- results[-c(1), ]

####

meta_quasipoisson$mod<- "meta_quasipoisson"
meta_quasipoisson_fourier$mod<-"meta_quasipoisson_fourier"
meta_mod_qp_ext$mod<-"meta_mod_qp_ext"
meta_mod_qp_ext_f$mod<-"meta_mod_qp_ext_f"

meta_nb$mod<-"meta_nb"
meta_nbf$mod<-"meta_nbf"
meta_mod_nb_ext$mod<-"meta_mod_nb_ext"
meta_mod_nb_ext_f$mod<-"meta_mod_nb_ext_f"

dataset<- rbind(meta_quasipoisson, meta_quasipoisson_fourier,
                meta_mod_qp_ext, meta_mod_qp_ext_f,
                meta_nb, meta_nbf, meta_mod_nb_ext,
                meta_mod_nb_ext_f)

models<-c("meta_quasipoisson", "meta_quasipoisson_fourier",
          "meta_mod_qp_ext", "meta_mod_qp_ext_f",
          "meta_nb", "meta_nbf", "meta_mod_nb_ext",
          "meta_mod_nb_ext_f")

meta<- data.frame("model"=1, "rr"=1, "lower"=1, "upper"=1)

for (i in models){
  
  df<- dataset %>%
    filter(mod==i) %>%
    filter(rr>0 & lwr>0 & upr>0)
  df<- df[-c(1), ]
  
  logRR <- log(df$rr)
  loglower <- log(df$lwr)
  logupper <- log(df$upr)
  
  graph1<- metagen(logRR, 
                   lower=loglower,
                   upper = logupper,
                   studlab = country,
                   method.tau = "REML",
                   sm = "RR",
                   text.fixed = "Fixed effects model",
                   data = df)
  
  meta_b<- data.frame("model"=1, "rr"=1, "lower"=1, "upper"=1)
  est.fixed <- data.frame(exp(unlist(summary(graph1)$fixed)))
  meta_b[1,1]<-i
  meta_b[1,2]<-round(est.fixed[1,1], 2)
  meta_b[1,3]<-round(est.fixed[3,1], 2)
  meta_b[1,4]<-round(est.fixed[4,1], 2)
  
  meta<-rbind(meta, meta_b)
}

meta<- meta[-c(1), ]

meta$m<- c("QP", "QP + F", "QP Ext", "QP + F Ext", "NB", "NB + F", "NB Ext", "NB + F Ext")

meta<- meta[, -c(1)]
colnames(meta)[4]<-"model"

everything<- merge(meta, results, all = T, by = "model")
return(everything)

}
