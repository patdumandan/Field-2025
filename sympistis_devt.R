require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)

filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\TEMPNET\\2026\\sympistis_development"

filename = paste(filepath, '\\sympistis_growth','.csv', sep = '')

sym_data=read.csv(filename,header=T, sep=",", dec = ",", na.strings = c("NA", ""))

sym_data$weight_1=as.numeric(as.character(sym_data$weight_1))
sym_data$weight_2=as.numeric(as.character(sym_data$weight_2))
sym_data$weight_3=as.numeric(as.character(sym_data$weight_3))

sym_dat=sym_data%>%
  mutate(weight_change1=weight_2/weight_1,
         weight_change2=weight_3/weight_2)%>%
  mutate(temperature=case_when(House==9 & Cold.Warm== "C" ~ "17",
                               House==9 & Cold.Warm== "W" ~ "34",
                               House==3 & Cold.Warm== "W" ~ "18",
                               House==3 & Cold.Warm== "C" ~ "7",
                               House==11 & Cold.Warm== "C" ~ "18",
                               House==11 & Cold.Warm== "W" ~ "27",
                               House==2 & Cold.Warm== "C" ~ "9",
                               House==2 & Cold.Warm== "W" ~ "29"))

sym_dat$temperature=as.integer(as.character(sym_dat$temperature))

sym_dat1=sym_dat%>%filter(!is.na(Cold.Warm))

require(ggplot2)

ggplot(sym_dat1, aes(x=temperature, y=weight_change1, col=as.factor(Cold.Warm)))+
  geom_point()+
  theme_classic()+ylab("relative change (weight)")
