moz1=read.csv("C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2025\\Field-2025\\raw_data\\Aedes\\Aedes-devt-rate_2025.csv", 
              header=T, sep=",")

require(dplyr)
require(tidyr)


moz_dat=moz1%>%
  pivot_longer(cols=3:13, names_to = "date")%>%
  rename(status=value)%>%
  mutate(month=substr(date, 2, 3),
         day=substr(date, 5,6),
         year=substr(date, 8, 9),
         Year=case_match(year, "25"~"2025"),
         Date=lubridate::make_date(Year, month, day))%>%
  select(-date)

moz_dat$month=as.integer(moz_dat$month)
moz_dat$day=as.integer(moz_dat$day)
moz_dat$year=as.integer(moz_dat$year)
moz_dat$Date=as.Date(moz_dat$Date)


moz_all= moz_dat %>%
  group_by(Temperature, Date, status) %>%
  mutate(Count = n()) %>%
  ungroup() %>%
  group_by(Temperature, Date) %>%
  mutate(Total = n()) %>%
  group_by(Temperature, Date, status) %>%
  mutate(CumSum = cumsum(1),
         CumFrac = CumSum / Total) %>%
  drop_na()

require(ggplot2)


ggplot(moz_all, aes(x=Date, y=CumFrac, fill=status))+
  geom_col()+#geom_line()+
  theme_classic()+
  facet_wrap(~Temperature)+
  ylab("cumulative fractions")
