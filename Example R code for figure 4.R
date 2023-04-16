library(ggplot2)
library(ggmap)
library(rgdal)
library(maps)
library(dplyr)
library(reshape2)
library(stringr)

#-------Example R code for figure 4--------

# prevalence
ggplot(data = data_all[data_all$location_name %in% c('Low SDI','Low-middle SDI','Middle SDI','High-middle SDI','High SDI')&
                         data_all$sex_name=='Both'&
                         data_all$measure_name=='Prevalence'&
                         data_all$metric_name=='Rate'&
                         data_all$age_name=='Age-standardized',
                       c('location_name','year','val_m')])+
  geom_line(aes(year,val_m*100,
                color= factor(location_name,levels = c('Low SDI','Low-middle SDI','Middle SDI','High-middle SDI','High SDI'))),
            size=1)+
  scale_color_brewer(palette = 'Set1')+
  theme_bw()+
  ylab('Age-standarized prevalence rate')+
  xlab('Year')+
  scale_x_continuous(limits = c(1990,2019))+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black",size = 0.5),
        axis.title = element_text(size = 11),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=8),
        legend.title = element_blank(),
        legend.text= element_text(size=10),
        legend.position = 'none')
