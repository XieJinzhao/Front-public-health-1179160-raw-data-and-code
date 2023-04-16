library(ggplot2)
library(ggmap)
library(rgdal)
library(maps)
library(dplyr)
library(reshape2)
library(stringr)

#-------Example R code for figure 3--------

# prevalence
prevalence_rate_23_region_all <- data_all[data_all$location_name %in% c(GBD_21_region$location_name,'Global')&
                                            data_all$sex_name=='Both'&
                                            data_all$measure_name=='Prevalence'&
                                            data_all$metric_name=='Rate'&
                                            data_all$age_name=='Age-standardized',
                                          c('location_name','year','val_m')]

names(prevalence_rate_23_region_all)[3] <- 'prevalence_rate'

# merge data for graphing
fig_SDI_all_year <- merge(incidence_rate_23_region_all,prevalence_rate_23_region_all,by=c('location_name','year'),sort = FALSE)
fig_SDI_all_year <- merge(fig_SDI_all_year,death_rate_23_region_all,by=c('location_name','year'),sort = FALSE)
fig_SDI_all_year <- merge(fig_SDI_all_year,DALY_rate_23_region_all,by=c('location_name','year'),sort = FALSE)
unique(fig_SDI_all_year$location_name)[which(!unique(fig_SDI_all_year$location_name) %in% unique(SDI$location_name))]

fig_SDI_all_year$location_name[fig_SDI_all_year$location_name=="Central Sub-Saharan Africa"]<-"Central sub-Saharan Africa"
fig_SDI_all_year$location_name[fig_SDI_all_year$location_name=="Eastern Sub-Saharan Africa"]<-"Eastern sub-Saharan Africa"
fig_SDI_all_year$location_name[fig_SDI_all_year$location_name=="Southern Sub-Saharan Africa"]<-"Southern sub-Saharan Africa"
fig_SDI_all_year$location_name[fig_SDI_all_year$location_name=="Western Sub-Saharan Africa"]<-"Western sub-Saharan Africa"

fig_SDI_all_year <- merge(fig_SDI_all_year,SDI,by=c('location_name','year'),all.x = TRUE, sort = FALSE)
fig_SDI_all_year <- fig_SDI_all_year[!duplicated(fig_SDI_all_year),]

fig_SDI_all_year$location_name <- factor(fig_SDI_all_year$location_name, 
                                         levels = c(name_order_table1$location_name[c(1,7:23)],
                                                    "Central sub-Saharan Africa","Eastern sub-Saharan Africa",
                                                    "Southern sub-Saharan Africa","Western sub-Saharan Africa"))

# Figure 3
ggplot(fig_SDI_all_year, aes(SDI,prevalence_rate*100))+
  geom_point(aes(color = location_name, shape= location_name))+
  scale_shape_manual(values = c(rep(1,22))) + 
  geom_smooth(colour='#4161AC',stat = "smooth",method='loess',se=T, alpha=0.2)+
  ylab('Age-standardized prevalence rate')+
  ylim(c(0,40))+
  theme_bw()+
  theme(plot.background = element_blank(),
        panel.grid =element_blank(),
        axis.line = element_line(colour = "black",size = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size=10),
        panel.border = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=12),
        legend.position = 'none')+
  guides(colour=guide_legend(override.aes = list(size=3)))

