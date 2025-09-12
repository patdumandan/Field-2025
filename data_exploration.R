#here, you open the "Processed file/document"

#Data####
filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2025\\Field-2025\\raw_data"

filename = paste(filepath, '\\ecophys_dat','.csv', sep = '')

#Locomotion####
##wolf spider####
cols=c("high arctic"= "navyblue", "low arctic"="orange")

ws_dat=ecophys_dat%>%filter(Taxon=="wolf_spider")

ws_dat$Time_sec[is.na(ws_dat$Time_sec)] <- 0
ws_dat$Distance_cm[is.na(ws_dat$Distance_cm)] <- 0

ws_plot=ggplot(ws_dat, aes(x=loc_temp, y=speed, col=area))+
  geom_point()+ 
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spider")+
  geom_vline(xintercept=42, lty=2)+
  scale_color_manual(values=cols)


ggplot(ws_dat, aes(x=loc_temp, y=speed, col=area))+
  geom_point()+ 
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spider (truncated)")+
  scale_color_manual(values=cols)


##weevil####
wv_dat=ecophys_dat%>%filter(Taxon=="weevil")

wv_plot=ggplot(wv_dat, aes(x=loc_temp, y=speed, col=area))+
  geom_point(aes(col=area))+ 
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("weevil")+ geom_vline(xintercept=38, lty=2)+
  scale_color_manual(values=cols)

##muscid####
mu_dat=ecophys_dat%>%filter(Taxon=="muscids")
zacmu_dat=ecophys_dat%>%filter(Taxon=="muscids",loc_temp<38)

mu_plot=ggplot(mu_dat, aes(x=loc_temp, y=speed, col=area))+
  geom_point(aes(col=area))+ geom_vline(xintercept=38, lty=2)+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("muscids")+
  scale_color_manual(values=cols)

##mosquito####
mos_dat=ecophys_dat%>%filter(Taxon=="mosquito")

mo_plot=ggplot(mos_dat, aes(x=loc_temp, y=speed, col=area))+
  geom_point(aes(col=area))+
  geom_smooth(method="gam")+
  theme_classic()+ geom_vline(xintercept=38, lty=2)+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("mosquito")+
  scale_color_manual(values=cols)

##craneflies####
cran_dat=ecophys_dat%>%filter(Taxon=="craneflies")

cran_plot=ggplot(cran_dat, aes(x=loc_temp, y=speed, col=Location))+
  geom_point(aes(col=Location))+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("craneflies")+
  scale_color_manual(values=cols)

ggarrange(ws_plot,wv_plot,mu_plot,mo_plot, nrow=2, ncol=2)

#HKDT####

hkdt_dat=ecophys_dat%>%filter(test=="HKDT", Temp%in%c(30:55))%>%
  mutate(
    hkdt_temp_range=case_when(
      Temp >= 25   & Temp < 30    ~ "25",
      Temp >= 30   & Temp < 35    ~ "30",
      Temp >= 35   & Temp < 40    ~ "35",
      Temp >= 40   & Temp < 45    ~ "40",
      Temp >= 45   & Temp < 50    ~ "45",
      Temp >= 50   & Temp < 55    ~ "50"),
    acclim_type=case_when(Acclim_temp<5~"cold",
                               Acclim_temp%in%c(6:19) ~"ambient",
                               Acclim_temp>20 ~"hot",
                               Acclim_temp= NA ~"ambient"))%>%
  filter(acclim_type%in%c("cold", "hot"))

hkdt_dat$end_time_s=as.numeric(hkdt_dat$end_time_s)
hkdt_dat$end_time_m=hkdt_dat$end_time_s/60

ggplot(hkdt_dat, aes(x=hkdt_temp_range, y=end_time_m))+
  geom_boxplot(aes(fill=acclim_type))+geom_jitter(size = 0.5, width = 0.2, height = 0)+facet_wrap(~Taxon)+
  theme_classic()+ylab("HKDT (min)")+xlab("Temperature (Celsius)")+
  scale_fill_manual(values=c("cold"="blue", "hot"="red"))


hkdt_summary_dat=hkdt_dat%>%
  group_by(Location, Taxon, hkdt_temp_range, acclim_type)%>%
  summarise(sample_size=n())

