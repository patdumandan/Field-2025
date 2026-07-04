require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)

filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\TEMPNET\\2025\\Field-2025\\raw_data\\ecophysiology"

filename = paste(filepath, '\\ecophys_2025','.csv', sep = '')

ecophys_data=read.csv(filename,header=T, sep=",")

ecophys_data=ecophys_data%>%
  mutate(
    clean_date = stringr::str_remove_all(Date, "X") %>% str_replace_all("\\.", "-"),
    date = mdy(clean_date),
    year=year(date))

ecophys_dat=ecophys_data%>%
  mutate(movement = if_else(Distance_cm < 1, "no", "yes"),
           speed = case_when(
             Time_sec == 0 & Distance_cm == 0 ~ 0,
             Time_sec == 0 & Distance_cm > 0 ~ NA_real_,
             TRUE ~ Distance_cm / Time_sec),
        # Size_cat=case_when(Weight_g<0.04 ~"S",
         #                   Weight_g>0.04 ~"L"),
         temp_range=case_when(
           loc_temp >=-2.5   & loc_temp <= 2.5    ~ "0",
           loc_temp >= 2.6   & loc_temp <= 7.5    ~ "5",
           loc_temp >= 7.6   & loc_temp <= 12.5   ~ "10",
           loc_temp >= 12.6   & loc_temp <= 17.5    ~ "15",
           loc_temp >= 17.6   & loc_temp <= 22.5    ~ "20",
           loc_temp >= 22.6   & loc_temp <= 27.5    ~ "25",
           loc_temp >= 27.6  & loc_temp <= 32.5   ~ "30",
           loc_temp >= 32.6   & loc_temp <= 37.5    ~ "35",
           loc_temp >= 37.6  & loc_temp <= 42.5   ~ "40",
           loc_temp >= 42.6   & loc_temp <= 47.5    ~ "45",
           loc_temp >= 47.6  & loc_temp <= 52.5   ~ "50",
           loc_temp >= 52.6  & loc_temp <= 57.5   ~ "55",
           loc_temp >= 57.6  & loc_temp <= 62.5   ~ "60",
           loc_temp >= 62.6  & loc_temp <= 67.5   ~ "65"))%>%
  mutate(area=case_when(Location=="Nuuk" ~"low arctic",
                             Location== "Kobbefjord" ~ "low arctic",
                             Location== "Zackenberg" ~ "high arctic"))

write.csv(ecophys_dat, "ecophys_dat.csv")

#proceed to data_exploration.R file, if you want to visualize the data

#summary table for locomotion####

all_loc_dat=ecophys_dat%>%filter(!is.na(loc_temp))

loc_summary_dat=all_loc_dat%>%
  group_by(Location, Taxon, temp_range)%>%
  summarise(sample_size=n())

#summary table for CTmax####
filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2025\\Field-2025\\raw_data\\ecophysiology"

filename = paste(filepath, '\\CTmax_data','.csv', sep = '')

ctmax_data=read.csv(filename,header=T, sep=",")

ctmax_dat=ctmax_data%>%
  filter(!is.na(CTmax), !CTmax=="Dead")%>%
  mutate(area=case_when(Location=="Nuuk" ~"low arctic",
                        Location== "Kobbefjord" ~ "low arctic",
                        Location== "Zackenberg" ~ "high arctic"),
         CTmax=as.numeric(CTmax))%>%
  group_by(area, Species)%>%
  summarise(n=n(),
            mean_val=median(CTmax), sd_val=sd(CTmax))

#summary of experiments####


filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2025\\Field-2025\\raw_data"

filename = paste(filepath, '\\TEMPNET_metadata','.csv', sep = '')

meta_data=read.csv(filename,header=T, sep=",")

meta_dat=meta_data%>%
  group_by(Location, SiteLong,SiteLat, Experiment)%>%
  summarise(n=sum(SampleSize))
