require(dplyr)
require(tidyr)
require(ggplot2)

#wolf spiders
loc_dat=read.csv("C:\\pdumandanSLU\\PatD-SLU\\SLU\\Field-2025\\2025\\data\\raw_data\\locomotion_2025.csv",
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
ggplot(ws_dat_b, aes(x=Temperature, y=speed))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+
  ggtitle("wolf spider (large)")

ggplot(ws_dat_size, aes(x=Temperature, y=speed, col=Size_cat))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+
  ggtitle("wolf spider")

ggplot(ws_dat, aes(x=Temperature, y=speed))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+
  ggtitle("wolf spider (all)")

ggplot(wv_dat, aes(x=Temperature, y=speed))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+
  ggtitle("weevil")

ggplot(mu_dat, aes(x=Temperature, y=speed))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+
  ggtitle("muscids")
