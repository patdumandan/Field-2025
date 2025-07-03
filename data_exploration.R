require(dplyr)
require(tidyr)
require(ggplot2)

#wolf spiders
loc_dat=read.csv("C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2025\\Field-2025\\raw_data\\locomotion_2025.csv",
                header=T, sep=",")

loc_dat=loc_dat%>%
  mutate(movement=if_else(Distance_cm<1, "no", "yes"),
         speed=Distance_cm/Time_sec)

ws_dat=loc_dat%>%filter(Taxon=="wolf_spider")
wv_dat=loc_dat%>%filter(Taxon=="weevil")
mu_dat=loc_dat%>%filter(Taxon=="muscids")

ws_dat_b=ws_dat%>%filter(Size_cat=="L")
ws_dat_size=ws_dat%>%filter(Size_cat%in%c("L","S"))

#plot
ggplot(ws_dat_b, aes(x=loc_temp, y=speed))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spider (large)")

ggplot(ws_dat_size, aes(x=loc_temp, y=speed, col=Size_cat))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spider")

ggplot(ws_dat, aes(x=loc_temp, y=speed))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spider (all)")

ggplot(wv_dat, aes(x=loc_temp, y=speed))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("weevil")

ggplot(mu_dat, aes(x=loc_temp, y=speed))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("muscids")
