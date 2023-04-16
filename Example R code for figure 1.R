library(ggplot2)
library(ggmap)
library(rgdal)
library(maps)
library(dplyr)
library(reshape2)
library(stringr)

#-------Example R code for figure 1--------

# prevalence rates

# importing world map data
worldData <- map_data('world')

# organizing country names
fig_pre_rate <- prevalence_rate_co[,c(1:4)]
names(fig_pre_rate)[1] <- 'region'

fig_pre_rate$region[fig_pre_rate$region == 'United States of America'] = 'USA'
fig_pre_rate$region[fig_pre_rate$region == 'Russian Federation'] = 'Russia'
fig_pre_rate$region[fig_pre_rate$region == 'United Kingdom'] = 'UK'
fig_pre_rate$region[fig_pre_rate$region == 'Congo'] = 'Republic of Congo'
fig_pre_rate$region[fig_pre_rate$region == "Iran (Islamic Republic of)"] = 'Iran'
fig_pre_rate$region[fig_pre_rate$region == "Democratic People's Republic of Korea"] = 'North Korea'
fig_pre_rate$region[fig_pre_rate$region == "Taiwan (Province of China)"] = 'Taiwan'
fig_pre_rate$region[fig_pre_rate$region == "Republic of Korea"] = 'South Korea'
fig_pre_rate$region[fig_pre_rate$region == "United Republic of Tanzania"] = 'Tanzania'
fig_pre_rate$region[fig_pre_rate$region == "C?te d'Ivoire"] = 'Saint Helena'
fig_pre_rate$region[fig_pre_rate$region == "Bolivia (Plurinational State of)"] = 'Bolivia'
fig_pre_rate$region[fig_pre_rate$region == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
fig_pre_rate$region[fig_pre_rate$region == "Czechia"] = 'Czech Republic'
fig_pre_rate$region[fig_pre_rate$region == "Republic of Moldova"] = 'Moldova'
fig_pre_rate$region[fig_pre_rate$region == "Viet Nam"] = 'Vietnam'
fig_pre_rate$region[fig_pre_rate$region == "Lao People's Democratic Republic"] = 'Laos'
fig_pre_rate$region[fig_pre_rate$region == "Syrian Arab Republic"] = 'Syria'
fig_pre_rate$region[fig_pre_rate$region == "North Macedonia"] = 'Macedonia'
fig_pre_rate$region[fig_pre_rate$region == "Micronesia (Federated States of)"] = 'Micronesia'
fig_pre_rate$region[fig_pre_rate$region == "Macedonia"] = 'North Macedonia'
fig_pre_rate$region[fig_pre_rate$region == "Trinidad and Tobago"] = 'Trinidad'
fig_pre_rate <- rbind(fig_pre_rate,fig_pre_rate[fig_pre_rate$region == "Trinidad",])
fig_pre_rate$region[fig_pre_rate$region == "Trinidad"] = 'Tobago'
fig_pre_rate$region[fig_pre_rate$region == "Cabo Verde"] = 'Cape Verde'
fig_pre_rate$region[fig_pre_rate$region == "United States Virgin Islands"] = 'Virgin Islands'
fig_pre_rate$region[fig_pre_rate$region == "Antigua and Barbuda"] = 'Antigu'
fig_pre_rate <- rbind(fig_pre_rate,fig_pre_rate[fig_pre_rate$region == "Antigu",])
fig_pre_rate$region[fig_pre_rate$region == "Antigu"] = 'Barbuda'
fig_pre_rate$region[fig_pre_rate$region == "Saint Kitts and Nevis"] = 'Saint Kitts'
fig_pre_rate <- rbind(fig_pre_rate,fig_pre_rate[fig_pre_rate$region == "Saint Kitts",])
fig_pre_rate$region[fig_pre_rate$region == "Saint Kitts"] = 'Nevis'
fig_pre_rate$region[fig_pre_rate$region == "C么te d'Ivoire"] = 'Ivory Coast'
fig_pre_rate$region[fig_pre_rate$region == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
fig_pre_rate <- rbind(fig_pre_rate,fig_pre_rate[fig_pre_rate$region == "Saint Vincent",])
fig_pre_rate$region[fig_pre_rate$region == "Saint Vincent"] = 'Grenadines'
fig_pre_rate$region[fig_pre_rate$region == "Eswatini"] = 'Swaziland'
fig_pre_rate$region[fig_pre_rate$region == "Brunei Darussalam"] = 'Brunei'

summary(unique(fig_pre_rate$region) %in% unique(worldData$region)) 

# merge map data and GBD data
fig_pre_rate_total <- full_join(worldData, fig_pre_rate, by='region')
fig_pre_rate_total <- fig_pre_rate_total[fig_pre_rate_total$region %in% fig_pre_rate$region, ]

# figure 1A
summary(fig_pre_rate_total$val_m)
quantile(fig_pre_rate_total$val_m, c(0.2,0.4,0.6,0.8), na.rm = TRUE)

fig_pre_rate_total <- fig_pre_rate_total %>%
  mutate(val2 = cut(val_m, breaks = c(0,0.02,0.04,0.06,0.10,0.20, max(fig_pre_rate_total$val_m,na.rm = TRUE)),
                    labels = c("0~","2~","4~","6~","10~",'20~'),
                    include.lowest = T,right = F))
ggplot()+
  geom_polygon(data=fig_pre_rate_total,
               aes(x=long, y=lat, group = group, fill=val2),
               colour="black",size = .12) + 
  scale_fill_brewer(palette = "Blues")+
  theme_void()+labs(x="", y="")+
  guides(fill = guide_legend(title='Prevalence rate\n(per 10,000,000 individuals)'))+
  theme(legend.position = c(0.13,0.25),
        legend.key.size = unit(12, "pt"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# calculating EAPC for prevalence
EAPC_prevalence_all <- subset(data_all, age_name=='Age-standardized' & metric_name== 'Rate' & measure_name=='Prevalence' & sex_name=='Both',
                              c('location_name','year','val_m'))

EAPC_prevalence_all_cal <- data.frame(location_name=unique(EAPC_prevalence_all$location_name),
                                      EAPC=rep(0,times=length(unique(EAPC_prevalence_all$location_name))),
                                      LCI=rep(0,times=length(unique(EAPC_prevalence_all$location_name))),
                                      UCI=rep(0,times=length(unique(EAPC_prevalence_all$location_name)))
) 

for (i in 1:length(unique(EAPC_prevalence_all$location_name))){  
  name <- EAPC_prevalence_all_cal[i,1]
  mod_simp_reg<-lm(log(EAPC_prevalence_all[EAPC_prevalence_all$location_name==name,]$val_m)~year,
                   data=EAPC_prevalence_all[EAPC_prevalence_all$location_name==name,])
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  lower <- (exp(confint(mod_simp_reg)[2,1])-1)*100
  upper <- (exp(confint(mod_simp_reg)[2,2])-1)*100
  EAPC_prevalence_all_cal[EAPC_prevalence_all_cal$location_name==name,2]<-estimate
  EAPC_prevalence_all_cal[EAPC_prevalence_all_cal$location_name==name,3]<-lower
  EAPC_prevalence_all_cal[EAPC_prevalence_all_cal$location_name==name,4]<-upper
}

EAPC_prevalence_all_cal$EAPC_prevalence_all_rate <- paste(round(EAPC_prevalence_all_cal$LCI,2),round(EAPC_prevalence_all_cal$UCI,2),sep = '-')
EAPC_prevalence_all_cal$EAPC_prevalence_all_rate <- paste0('(',EAPC_prevalence_all_cal$EAPC_prevalence_all_rate)
EAPC_prevalence_all_cal$EAPC_prevalence_all_rate <- paste0(EAPC_prevalence_all_cal$EAPC_prevalence_all_rate,')')
EAPC_prevalence_all_cal$EAPC_prevalence_all_rate <- paste(round(EAPC_prevalence_all_cal$EAPC,2),EAPC_prevalence_all_cal$EAPC_prevalence_all_rate,sep = ' ')

# EAPC data for countries
EAPC_prevalence_all_cal <- EAPC_prevalence_all_cal[EAPC_prevalence_all_cal$location_name %in% prevalence_case_co$location_name, ]

# organizing country names
fig_pre_EAPC <- EAPC_prevalence_all_cal[,c(1:4)]
names(fig_pre_EAPC)[1] <- 'region'

fig_pre_EAPC$region[fig_pre_EAPC$region == 'United States of America'] = 'USA'
fig_pre_EAPC$region[fig_pre_EAPC$region == 'Russian Federation'] = 'Russia'
fig_pre_EAPC$region[fig_pre_EAPC$region == 'United Kingdom'] = 'UK'
fig_pre_EAPC$region[fig_pre_EAPC$region == 'Congo'] = 'Republic of Congo'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Iran (Islamic Republic of)"] = 'Iran'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Democratic People's Republic of Korea"] = 'North Korea'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Taiwan (Province of China)"] = 'Taiwan'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Republic of Korea"] = 'South Korea'
fig_pre_EAPC$region[fig_pre_EAPC$region == "United Republic of Tanzania"] = 'Tanzania'
fig_pre_EAPC$region[fig_pre_EAPC$region == "C?te d'Ivoire"] = 'Saint Helena'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Bolivia (Plurinational State of)"] = 'Bolivia'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Czechia"] = 'Czech Republic'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Republic of Moldova"] = 'Moldova'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Viet Nam"] = 'Vietnam'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Lao People's Democratic Republic"] = 'Laos'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Syrian Arab Republic"] = 'Syria'
fig_pre_EAPC$region[fig_pre_EAPC$region == "North Macedonia"] = 'Macedonia'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Micronesia (Federated States of)"] = 'Micronesia'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Macedonia"] = 'North Macedonia'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Trinidad and Tobago"] = 'Trinidad'
fig_pre_EAPC <- rbind(fig_pre_EAPC,fig_pre_EAPC[fig_pre_EAPC$region == "Trinidad",])
fig_pre_EAPC$region[fig_pre_EAPC$region == "Trinidad"] = 'Tobago'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Cabo Verde"] = 'Cape Verde'
fig_pre_EAPC$region[fig_pre_EAPC$region == "United States Virgin Islands"] = 'Virgin Islands'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Antigua and Barbuda"] = 'Antigu'
fig_pre_EAPC <- rbind(fig_pre_EAPC,fig_pre_EAPC[fig_pre_EAPC$region == "Antigu",])
fig_pre_EAPC$region[fig_pre_EAPC$region == "Antigu"] = 'Barbuda'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Saint Kitts and Nevis"] = 'Saint Kitts'
fig_pre_EAPC <- rbind(fig_pre_EAPC,fig_pre_EAPC[fig_pre_EAPC$region == "Saint Kitts",])
fig_pre_EAPC$region[fig_pre_EAPC$region == "Saint Kitts"] = 'Nevis'
fig_pre_EAPC$region[fig_pre_EAPC$region == "C么te d'Ivoire"] = 'Ivory Coast'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
fig_pre_EAPC <- rbind(fig_pre_EAPC,fig_pre_EAPC[fig_pre_EAPC$region == "Saint Vincent",])
fig_pre_EAPC$region[fig_pre_EAPC$region == "Saint Vincent"] = 'Grenadines'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Eswatini"] = 'Swaziland'
fig_pre_EAPC$region[fig_pre_EAPC$region == "Brunei Darussalam"] = 'Brunei'

summary(unique(fig_pre_EAPC$region) %in% unique(worldData$region))

# merge map data and GBD data
fig_pre_EAPC_total <- full_join(worldData, fig_pre_EAPC, by='region')
fig_pre_EAPC_total <- fig_pre_EAPC_total[fig_pre_EAPC_total$region %in% fig_pre_EAPC$region, ]

# Figure 1B
summary(fig_pre_EAPC_total$EAPC)
quantile(fig_pre_EAPC_total$EAPC, c(0.2,0.4,0.6,0.8), na.rm = TRUE)
fig_pre_EAPC_total <- fig_pre_EAPC_total %>%
  mutate(val2 = cut(EAPC, breaks = c(-5,-2,0,2,4,6, max(fig_pre_EAPC_total$EAPC,na.rm = TRUE)),
                    labels = c("-5~","-2~","0~","2~","4~",'6~'),
                    include.lowest = T,right = F))
ggplot()+
  geom_polygon(data=fig_pre_EAPC_total,
               aes(x=long, y=lat, group = group, fill=val2),
               colour="black",size = .12) + 
  scale_fill_brewer(palette = "Blues")+
  theme_void()+labs(x="", y="")+
  guides(fill = guide_legend(title='EAPC (%)'))+
  theme(legend.position = c(0.13,0.25),
        legend.key.size = unit(12, "pt"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))