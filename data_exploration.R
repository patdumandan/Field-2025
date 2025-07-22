require(dplyr)
require(tidyr)
require(ggplot2)

#wolf spiders
filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2025\\Field-2025\\raw_data"

filename = paste(filepath, '\\locomotion_2025','.csv', sep = '')

loc_dat=read.csv(filename,header=T, sep=",")

#locomotion####
loc_dat=loc_dat%>%
  mutate(movement=if_else(Distance_cm<1, "no", "yes"),
         speed=Distance_cm/Time_sec,
         Size_cat=case_when(Weight_g<0.04 ~"S",
                            Weight_g>0.04 ~"L"))

##wolf spider####

ws_dat=loc_dat%>%filter(Taxon=="wolf_spider", loc_temp<45)

ws_dat$Time_sec[is.na(ws_dat$Time_sec)] <- 0
ws_dat$Distance_cm[is.na(ws_dat$Distance_cm)] <- 0

ggplot(ws_dat, aes(x=loc_temp, y=speed, col=Location))+
  geom_point(aes(col=Location))+ 
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spider (all)")

##weevil####
wv_dat=loc_dat%>%filter(Taxon=="weevil")

ggplot(wv_dat, aes(x=loc_temp, y=speed))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("weevil")

##muscid####
mu_dat=loc_dat%>%filter(Taxon=="muscids")

ggplot(mu_dat, aes(x=loc_temp, y=speed, col=Location))+
  geom_point(aes(col=Location))+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("muscids")

#HKDT####

hkdt_dat=loc_dat%>%filter(test=="HKDT", Temp%in%c(35:45))%>%
  mutate(acclim_type=case_when(Acclim_temp<5~"cold",
                               Acclim_temp%in%c(6:19) ~"ambient",
                               Acclim_temp>25 ~"hot",
                               Acclim_temp= NA ~"ambient"))%>%
  filter(acclim_type%in%c("cold", "hot"))

hkdt_dat$end_time_s=as.numeric(hkdt_dat$end_time_s)


ggplot(hkdt_dat, aes(x=Temp, y=end_time_s, ))+
  geom_boxplot(aes(fill=acclim_type))+facet_wrap(~Taxon)+
  theme_classic()+ylab("HKDT")+xlab("Temperature")+
  scale_fill_manual(values=c("cold"="blue", "hot"="red"))
