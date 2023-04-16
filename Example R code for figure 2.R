library(ggplot2)
library(ggmap)
library(rgdal)
library(maps)
library(dplyr)
library(reshape2)
library(stringr)

#-------Example R code for figure 2--------

# prevalence

data_age_sex$age_name <- factor(data_age_sex$age_name)
data_age_sex$sex_name <- factor(data_age_sex$sex_name, levels = c('Male','Female','Both'))

ggplot()+
  geom_bar(data=data_age_sex[data_age_sex$sex_name!='Both'&
                               data_age_sex$measure_name=='Prevalence'&
                               data_age_sex$metric_name=='Number'&
                               data_age_sex$age_name!='All Ages'&data_age_sex$age_name!='Age-standardized',],
           aes(x=age_name, y=val_m, fill=sex_name),
           stat="identity", width = 0.6, position =  position_dodge(width=0.6), color='black',size=0.2)+
  geom_errorbar(data=data_age_sex[data_age_sex$sex_name!='Both'&
                                    data_age_sex$measure_name=='Prevalence'&
                                    data_age_sex$metric_name=='Number'&
                                    data_age_sex$age_name!='All Ages'&data_age_sex$age_name!='Age-standardized',],
                aes(x=age_name, y=val_m, group=sex_name, ymin=lower_m, ymax=upper_m),
                linetype=1,width=0.4, position =  position_dodge(width=0.6),size=0.2)+
  geom_line(data=data_age_sex[data_age_sex$sex_name!='Both'&
                                data_age_sex$measure_name=='Prevalence'&
                                data_age_sex$metric_name=='Rate'&
                                data_age_sex$age_name!='All Ages'&data_age_sex$age_name!='Age-standardized',],
            aes(x=age_name, y=val_m*1000,group=sex_name,color=sex_name),
            size=1)+
  geom_ribbon(data=data_age_sex[data_age_sex$sex_name!='Both'&
                                  data_age_sex$measure_name=='Prevalence'&
                                  data_age_sex$metric_name=='Rate'&
                                  data_age_sex$age_name!='All Ages'&data_age_sex$age_name!='Age-standardized',],
              aes(x=age_name, y=val_m*1000,
                  ymin=lower_m*1000, ymax=upper_m*1000,group=sex_name,fill=sex_name),
              alpha=0.5)+
  scale_color_manual(name='', values = c(Male='#2972AC',Female='#F15C3F'),labels=c('Male (rate and 95% UI)','Female (rate and 95% UI)'))+
  scale_fill_manual(name='', values = c(Male='#2972AC',Female='#F15C3F'),labels=c('Male (case and 95% UI)','Female (case and 95% UI)'))+
  scale_y_continuous(limits = c(0,3700), sec.axis = sec_axis(~./1000, name = 'Age-standardized prevalence rate',breaks = c(0,1,2,3), 
                                                             labels = c('0','100','200','300')),expand = c(0, 0))+
  scale_x_discrete(labels=c('15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95-'))+
  xlab('Age (years)')+
  ylab('Prevalent cases') +
  theme_bw()+
  theme(plot.background = element_blank(),
        panel.grid =element_blank(),
        panel.border = element_rect(colour = "black",size = 0.5),
        axis.text.x = element_text(hjust=1, vjust=1, size = 10, angle = 45),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=12),
        axis.title.y.right = element_text(vjust = 2),
        axis.title.y.left  = element_text(vjust = 2),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text= element_text(size=12),
        legend.key = element_blank())
