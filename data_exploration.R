#here, you open the "Processed file/document"

filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2025\\Field-2025\\raw_data"

filename = paste(filepath, '\\loc_dat','.csv', sep = '')

##wolf spider####

ws_dat=loc_dat%>%filter(Taxon=="wolf_spider")

ws_dat$Time_sec[is.na(ws_dat$Time_sec)] <- 0
ws_dat$Distance_cm[is.na(ws_dat$Distance_cm)] <- 0

zacws_dat=loc_dat%>%filter(Taxon=="muscids", Location=="Zackenberg")

a1=ggplot(ws_dat, aes(x=loc_temp, y=speed, col=Location))+
  geom_point(aes(col=Location))+ 
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spider (all)")+
  geom_vline(xintercept=42, lty=2)

a2=ggplot(ws_dat, aes(x=loc_temp, y=speed, col=Location))+
  geom_point(aes(col=Location))+ 
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spider (truncated)")

ggarrange(a1,a2)

##weevil####
wv_dat=loc_dat%>%filter(Taxon=="weevil")

ggplot(wv_dat, aes(x=loc_temp, y=speed))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("weevil")

##muscid####
mu_dat=loc_dat%>%filter(Taxon=="muscids", loc_temp<43)
#zacmu_dat=loc_dat%>%filter(Taxon=="muscids", Location=="Zackenberg",loc_temp<45)

ggplot(mu_dat, aes(x=loc_temp, y=speed, col=Location))+
  geom_point(aes(col=Location))+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("muscids")

##mosquito####
mos_dat=loc_dat%>%filter(Taxon=="mosquito")

ggplot(mos_dat, aes(x=loc_temp, y=speed, col=Location))+
  geom_point(aes(col=Location))+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("mosquito")

#HKDT####

hkdt_dat=loc_dat%>%filter(test=="HKDT", Temp%in%c(35:45))%>%
  mutate(acclim_type=case_when(Acclim_temp<5~"cold",
                               Acclim_temp%in%c(6:19) ~"ambient",
                               Acclim_temp>25 ~"hot",
                               Acclim_temp= NA ~"ambient"))%>%
  filter(acclim_type%in%c("cold", "hot"))

hkdt_dat$end_time_s=as.numeric(hkdt_dat$end_time_s)
hkdt_dat$end_time_m=hkdt_dat$end_time_s/60

ggplot(hkdt_dat, aes(x=Temp, y=end_time_m, ))+
  geom_boxplot(aes(fill=acclim_type))+facet_wrap(~Taxon)+
  theme_classic()+ylab("HKDT (min)")+xlab("Temperature (Celsius)")+
  scale_fill_manual(values=c("cold"="black", "hot"="grey"))
