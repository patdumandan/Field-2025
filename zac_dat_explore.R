require(tidyr)
require(dplyr)

##data####
zacdat=ecophys_dat%>%filter(Location=="Zackenberg")

zacdat1=zacdat%>%filter(!is.na(Distance_cm))%>%
  group_by(Taxon, temp_range)%>%
  summarise(sample_size=n())

zacdat=ecophys_dat%>%filter(!is.na(Distance_cm))%>%
  filter(Location=="Zackenberg",
         !Taxon%in%c("sympistis_larva", "wooly_moth"))

zacdat$Time_sec[is.na(zacdat$Time_sec)] <- 0
zacdat$Distance_cm[is.na(zacdat$Distance_cm)] <- 0

ggplot(zacdat, aes(x=loc_temp, y=speed))+
  geom_point(aes(color=as.factor(year)))+
  geom_smooth(method="gam")+ facet_wrap(~Taxon)+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("Zackenberg")
 # geom_vline(xintercept=42, lty=2)

#craneflies####
zaccran=zacdat%>%filter(Taxon=="craneflies")

ggplot(zaccran, aes(x=loc_temp, y=speed))+
  geom_point(aes(color=as.factor(year)))+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("craneflies")
 # geom_vline(xintercept=42, lty=2)

#empids####
zacemp=zacdat%>%filter(Taxon=="empids")

ggplot(zacemp, aes(x=loc_temp, y=speed))+
  geom_point(aes(color=as.factor(year)))+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("empids")
 # geom_vline(xintercept=42, lty=2)

#mosquitoes####
zacmoz=zacdat%>%filter(Taxon=="mosquito", year=="2026")

median(zacmoz$speed, na.rm=T)

zacmoz1=zacmoz%>%filter(!is.na(Distance_cm))%>%
  group_by(temp_range)%>%
  summarise(sample_size=n())

ggplot(zacmoz, aes(x=loc_temp, y=speed))+
  geom_point(aes(color=as.factor(year)))+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("mosquitoes")

#muscids####
zacmus=zacdat%>%filter(Taxon=="muscids")

ggplot(zacmus, aes(x=loc_temp, y=speed))+
  geom_point(aes(color=as.factor(year)))+ 
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("muscids")
#  geom_vline(xintercept=39, lty=2)

#wolf spiders####
zacws_summary=zacdat1%>%filter(Taxon=="wolf_spider")

zacws=zacdat%>%filter(Taxon=="wolf_spider", !Acclim_time>1)

ggplot(zacws, aes(x=loc_temp, y=speed))+
  geom_point(aes(color=as.factor(year)))+ xlim(0,65)+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spiders")+
  geom_vline(xintercept=42, lty=2)

+##acclimation effect; speed at >45C####

zacws_at=zacws%>%filter(!Acclim_time=="", !Acclim_time<2)

ggplot(zacws_at,aes(x=Acclim_time, y=speed, col=as.factor(year)))+
  geom_boxplot()+geom_jitter()+
  theme_classic()+ggtitle("wolf spider speed >45C")

zacat=zacws%>%filter(!Acclim_time=="")%>%
  group_by(Acclim_time)%>%
  summarise(sample_size=n())

#3 taxa

zac3=zacdat%>%filter(Taxon%in%c("wolf_spider", "craneflies", "muscids"))                     


ggplot(zac3, aes(x=loc_temp, y=speed, col=Taxon))+
  geom_point(aes(color=as.factor(Taxon)))+ xlim(0,50)+
  ylim(0,10)+
  geom_smooth(method="gam", aes(col=Taxon))+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spiders")+
  geom_vline(xintercept=42, lty=2)
