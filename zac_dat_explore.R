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

ggplot(zacdat, aes(x=loc_temp, y=speed, color=as.factor(year)))+
  geom_point()+ facet_wrap(~Taxon)+
  geom_smooth(aes(group=as.factor(year)),method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("Zackenberg")+
  geom_vline(xintercept=42, lty=2)

#craneflies####
zaccran=zacdat%>%filter(Taxon=="craneflies")

ggplot(zaccran, aes(x=loc_temp, y=speed))+
  geom_point(aes(color=as.factor(year)))+ xlim(0,35)+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("craneflies")+xlim(0,35)+
  geom_vline(xintercept=42, lty=2)

#empids####
zacemp=zacdat%>%filter(Taxon=="empids")

ggplot(zacemp, aes(x=loc_temp, y=speed))+
  geom_point(aes(color=as.factor(year)))+ xlim(0,35)+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("empids")+
  geom_vline(xintercept=42, lty=2)

#muscids####
zacmus=zacdat%>%filter(Taxon=="muscids")

ggplot(zacmus, aes(x=loc_temp, y=speed))+
  geom_point(aes(color=as.factor(year)))+ xlim(0,50)+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("muscids")+
  geom_vline(xintercept=39, lty=2)

#wolf spiders####
zacws_summary=zacdat1%>%filter(Taxon=="wolf_spider")

zacws=zacdat%>%filter(Taxon=="wolf_spider", !loc_temp>40)

ggplot(zacws, aes(x=loc_temp, y=speed))+
  geom_point(aes(color=as.factor(year)))+ xlim(0,50)+
  geom_smooth(method="gam")+
  theme_classic()+
  ylab("speed (cm/s)")+xlab("Temperature(C)")+
  ggtitle("wolf spiders")+
  geom_vline(xintercept=42, lty=2)
