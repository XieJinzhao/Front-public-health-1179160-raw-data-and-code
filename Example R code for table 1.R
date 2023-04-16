library(dplyr)
library(reshape2)
library(stringr)

#-------Example R code for table 1--------

# calculating disease burden due to the comorbidity by PAF
data$val_m <- data$val*data$PAF
data$upper_m <- data$upper*data$PAF
data$lower_m <- data$lower*data$PAF

# prevalence case
prevalence_case <- data[data$measure_name=='Prevalence'&
                          data$age_name=='All Ages'&
                          data$metric_name=='Number',c(4,20,21,22)]

prevalence_case$prevalence_case <- paste(round(prevalence_case$lower_m,1),round(prevalence_case$upper_m,1),sep='-')
prevalence_case$prevalence_case <- paste0('(', prevalence_case$prevalence_case)
prevalence_case$prevalence_case <- paste0(prevalence_case$prevalence_case,')')
prevalence_case$prevalence_case <- paste(round(prevalence_case$val_m,1), prevalence_case$prevalence_case, sep = ' ')

# prevalence rate
prevalence_rate <- data[data$measure_name=='Prevalence'&
                          data$age_name=='Age-standardized'&
                          data$metric_name=='Rate',c(4,20,21,22)]

prevalence_rate$prevalence_rate <- paste(round(prevalence_rate$lower_m,3),round(prevalence_rate$upper_m,3),sep='-')
prevalence_rate$prevalence_rate <- paste0('(', prevalence_rate$prevalence_rate)
prevalence_rate$prevalence_rate <- paste0(prevalence_rate$prevalence_rate,')')
prevalence_rate$prevalence_rate <- paste(round(prevalence_rate$val_m,3), prevalence_rate$prevalence_rate, sep = ' ')

# table 1
table1 <- merge(PAF[PAF$sex_name=='Both', c('location_name','location_id','PAF_percentage','PAF_percentage_upper','PAF_percentage_lower')],
                prevalence_case[,c(1,5)])
table1 <- merge(table1, prevalence_rate[,c(1,5)])
table1 <- merge(table1, incidence_case[,c(1,5)])
table1 <- merge(table1, incidence_rate[,c(1,5)])
table1 <- merge(table1, death_case[,c(1,5)])
table1 <- merge(table1, death_rate[,c(1,5)])
table1 <- merge(table1, DALY_case[,c(1,5)])
table1 <- merge(table1, DALY_rate[,c(1,5)])

table1$PAF_c <- paste(table1$PAF_percentage_lower, table1$PAF_percentage_upper, sep = '-')
table1$PAF_c <- paste0('(', table1$PAF_c)
table1$PAF_c <- paste0(table1$PAF_c,')')
table1$PAF_c <- paste(table1$PAF_percentage, table1$PAF_c, sep = ' ')

# calculating EAPC of prevalence
EAPC_prevalence <- subset(data_all, age_name=='Age-standardized' & metric_name== 'Rate' & measure_name=='Prevalence' & sex_name=='Both'
                          & data_all$location_name %in% table1_final$location_name, c('location_name','year','val_m'))


EAPC_prevalence_cal <- data.frame(location_name=unique(EAPC_prevalence$location_name),
                                  EAPC=rep(0,times=length(unique(EAPC_prevalence$location_name))),
                                  LCI=rep(0,times=length(unique(EAPC_prevalence$location_name))),
                                  UCI=rep(0,times=length(unique(EAPC_prevalence$location_name)))
) 


for (i in 1:length(unique(EAPC_prevalence$location_name))){  
  
  name <- EAPC_prevalence_cal[i,1]
  mod_simp_reg<-lm(log(EAPC_prevalence[EAPC_prevalence$location_name==name,]$val_m)~year,data=EAPC_prevalence[EAPC_prevalence$location_name==name,]) 
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100 
  lower <- (exp(confint(mod_simp_reg)[2,1])-1)*100
  upper <- (exp(confint(mod_simp_reg)[2,2])-1)*100
  EAPC_prevalence_cal[EAPC_prevalence_cal$location_name==name,2]<-estimate
  EAPC_prevalence_cal[EAPC_prevalence_cal$location_name==name,3]<-lower
  EAPC_prevalence_cal[EAPC_prevalence_cal$location_name==name,4]<-upper
  
}

EAPC_prevalence_cal$EAPC_prevalence_rate <- paste(sprintf('%0.2f',round(EAPC_prevalence_cal$LCI,2)),sprintf('%0.2f',round(EAPC_prevalence_cal$UCI,2)),sep = '-')
EAPC_prevalence_cal$EAPC_prevalence_rate <- paste0('(',EAPC_prevalence_cal$EAPC_prevalence_rate)
EAPC_prevalence_cal$EAPC_prevalence_rate <- paste0(EAPC_prevalence_cal$EAPC_prevalence_rate,')')
EAPC_prevalence_cal$EAPC_prevalence_rate <- paste(sprintf('%0.2f',round(EAPC_prevalence_cal$EAPC,2)),EAPC_prevalence_cal$EAPC_prevalence_rate,sep = ' ')
