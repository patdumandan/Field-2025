require(dplyr)
require(tidyr)
library(stringr)
require(lubridate)

#load data###
filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2025\\Field-2025\\raw_data\\Aedes"

filename = paste(filepath, '\\Aedes-devt-rate_2025','.csv', sep = '')

tempdat=paste(filepath, '\\T1-10C','.txt', sep = '')

moz1=read.csv(filename, header=T, sep=",", stringsAsFactors = F)
tempdat1=read.csv(tempdat, fileEncoding = "Latin1", header=T)

#reconfigure table for analysis###

moz_dat <- moz1 %>%
  pivot_longer(cols = 3:37, names_to = "date") %>% #make sure to increase cols end until end of experiment
  rename(status = value) %>%
  mutate(
    clean_date = stringr::str_remove_all(date, "X") %>% str_replace_all("\\.", "-"),
    Date = mdy(clean_date)) %>%
  select(-date, -clean_date)

moz_dat$Date=as.Date(moz_dat$Date)

library(dplyr)
library(tidyr)

moz_prop <- moz_dat %>%
  group_by(Temperature, Date, status) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = status, values_from = Count, values_fill = 0) %>%  # Fill NAs with 0
  rowwise() %>%
  mutate(Total = sum(c_across(3:6)),
         larva=L/Total,
         pupa=P/Total,
         adult=A/Total,
         dead=D/Total)%>%
  select(-L, -P, -D, -A)%>%
  pivot_longer(cols=4:7, names_to="stage")

#plot data###

require(ggplot2)

ggplot(moz_prop, aes(x=Date, y=value, fill=stage))+
  geom_col()+#geom_line()+
  theme_classic()+
  facet_wrap(~Temperature)+
  ylab("proportion")+
  scale_color_viridis_c()

moz_dat$Date=as.Date(moz_dat$Date)


moz_all= moz_dat %>%
  group_by(Temperature, Date, status) %>%
  mutate(Count = n()) %>%
  ungroup() %>%
  group_by(Temperature, Date) %>%
  mutate(Total = n()) %>%
  arrange(Temperature, Date)%>%
  group_by(Temperature, Date, status) %>%
  mutate(CumSum = cumsum(1),
         CumFrac = CumSum / Total) %>%
  drop_na()%>%
  select(Date, Temperature, CumFrac)

require(ggplot2)


ggplot(moz_all, aes(x=Date, y=CumFrac, fill=status))+
  geom_col()+#geom_line()+
  theme_classic()+
  facet_wrap(~Temperature)+
  ylab("cumulative fractions")
