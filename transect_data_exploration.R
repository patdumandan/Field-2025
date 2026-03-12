require(dplyr)
require(tidyr)

transect_dat2025=read.csv("C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2025\\Field-2025\\raw_data\\transect\\transect_2025.csv")
transect_dat2024=read.csv("C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2024\\Greenland_Data_TempGradient.csv")

transect2025_summary=transect_dat2025%>%
  drop_na()%>%
  group_by(Day, Site.Logger)%>%
  summarise(detects=length(which(Insect.Plant.Interactions!="NA")))

transect2024_summary=transect_dat2024%>%
  drop_na()%>%
  group_by(Day, Site.Logger)%>%filter(!Insect.Plant.Interactions=="No insects")%>%
  summarise(detects=length(which(Insect.Plant.Interactions!="NA")))

require(ggplot2)

ggplot(transect2025_summary,aes(x=Day, y=detects))+facet_wrap(~Site.Logger)+
  geom_col()+ggtitle("2025 transect")+theme_classic()

ggplot(transect2024_summary,aes(x=Day, y=detects))+facet_wrap(~Site.Logger)+
  geom_col()+ggtitle("2024 transect")+theme_classic()
