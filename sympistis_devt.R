require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)
require(ggpubr)

filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\TEMPNET\\2026\\sympistis_development"

filename = paste(filepath, '\\sympistis_growth_clean','.csv', sep = '')

sym_data=read.csv(filename,header=T, sep=",", dec = ",", na.strings = c("NA", ""))

sym_data$weight_1=as.numeric(as.character(sym_data$weight_1))
sym_data$weight_2=as.numeric(as.character(sym_data$weight_2))
sym_data$weight_3=as.numeric(as.character(sym_data$weight_3))
sym_data$weight_4=as.numeric(as.character(sym_data$weight_4))
sym_data$weight_5=as.numeric(as.character(sym_data$weight_5))
sym_data$weight_6=as.numeric(as.character(sym_data$weight_6))
sym_data$weight_7=as.numeric(as.character(sym_data$weight_7))
sym_data$weight_8=as.numeric(as.character(sym_data$weight_8))
sym_data$weight_9=as.numeric(as.character(sym_data$weight_9))
sym_data$weight_10=as.numeric(as.character(sym_data$weight_10))
sym_data$weight_11=as.numeric(as.character(sym_data$weight_11))
sym_data$weight_12=as.numeric(as.character(sym_data$weight_12))

sym_dat=sym_data%>%
  mutate(weight_change1=(weight_2/weight_1),
         weight_change2=weight_3/weight_2,
         weight_change3=weight_4/weight_3,
         weight_change4=weight_5/weight_4,
         weight_change5=weight_6/weight_5,
         weight_change6=weight_7/weight_6,
         weight_change7=weight_8/weight_7,
         weight_change8=weight_9/weight_8,
         weight_change9=weight_10/weight_9,
         weight_change10=weight_11/weight_10,
         weight_change11=weight_12/weight_11)%>%
  mutate(temperature=case_when(House==9 & Cold.Warm== "C" ~ "22",#changed to 22
                               House==9 & Cold.Warm== "W" ~ "34",
                               House==3 & Cold.Warm== "W" ~ "18",#18
                               House==3 & Cold.Warm== "C" ~ "7",
                               House==11 & Cold.Warm== "C" ~ "18",
                               House==11 & Cold.Warm== "W" ~ "27", #changed to 40
                               House==2 & Cold.Warm== "C" ~ "9",
                               House==2 & Cold.Warm== "W" ~ "29"))

sym_dat$Temp_new=ifelse(is.na(sym_dat$Temp_04Jul), sym_dat$temperature, sym_dat$Temp_04Jul)

#temp changes made on 04-Jul

sym_dat$temperature=as.integer(as.character(sym_dat$temperature))
sym_dat$Temp_new =as.integer(as.character(sym_dat$Temp_new))

sym_dat1=sym_dat%>%filter(!is.na(Cold.Warm))

sym_dat_weight=sym_dat1%>%
  select(-stage_1,-stage_2,-stage_3, -stage_4,-stage_5,
         -stage_6,-stage_7,-stage_8,-stage_9,-stage_10, -stage_11,-stage_12,
         -Temp_04Jul, -temp_change)%>%
  pivot_longer(cols=4:15, names_to = "growth")%>%
  group_by(House, Cold.Warm, Temp_new, ID)%>%
  mutate(time=seq(1:12))

sym_dat_wc=sym_dat1%>%
  select(-stage_1,-stage_2,-stage_3, -stage_4,-stage_5,
         -stage_6,-stage_7,-stage_8,-stage_9,-stage_10, -stage_11,-stage_12,
         -weight_1,-weight_2,-weight_3, -weight_4,-weight_5,
         -weight_6,-weight_7,-weight_8,-weight_9,-weight_10, -weight_11,-weight_12,
         -Temp_04Jul, -temp_change)%>%
  pivot_longer(cols=4:14, names_to = "weight_change", values_to = "ROC")%>%
  group_by(House, Cold.Warm, Temp_new, ID)%>%
  mutate(time=seq(1:11))


ggplot(sym_dat_weight, aes(x=time, y=value, col=as.factor(Temp_new)))+
  geom_point()+geom_smooth(method="loess")+
  theme_classic()+ylab("weight")+xlab("observation period")+
  ggtitle("development rate of Sympistis larvae")+
  scale_x_continuous(breaks=seq(1, 12, 1))+
  labs(color="temperature")

orig_temp=sym_dat1%>%filter(!Temp_new%in%c(22,40))

require(ggplot2)

g1=ggplot(sym_dat1, aes(x=Temp_new, y=weight_change1, col=as.factor(Temp_new)))+
  geom_point()+
  theme_classic()+ylab("relative change (weight)")+
  ggtitle("weight change 1")

g3=ggplot(sym_dat1, aes(x=Temp_new, y=weight_change3, col=as.factor(Temp_new)))+
  geom_point()+
  theme_classic()+ylab("relative change (weight)")+
  ggtitle("weight change 3")

g5=ggplot(sym_dat1, aes(x=Temp_new, y=weight_change5, col=as.factor(Temp_new)))+
  geom_point()+
  theme_classic()+ylab("relative change (weight)")+
  ggtitle("weight change 5")

g7=ggplot(sym_dat1, aes(x=Temp_new, y=weight_change7, col=as.factor(Temp_new)))+
  geom_point()+
  theme_classic()+ylab("relative change (weight)")+
  ggtitle("weight change 7")

g10=ggplot(sym_dat1, aes(x=Temp_new, y=weight_change10, col=as.factor(Temp_new)))+
  geom_point()+
  theme_classic()+ylab("relative change (weight)")+
  ggtitle("weight change 10")

ggarrange(g1,g3,g5,g10)
