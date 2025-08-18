require(dplyr)
require(tidyr)
require(ggplot2)

filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\fieldwork\\2025\\Field-2025\\raw_data"

filename = paste(filepath, '\\ecophys_2025','.csv', sep = '')

ecophys_data=read.csv(filename,header=T, sep=",")

ecophys_dat=ecophys_data%>%
  mutate(movement=if_else(Distance_cm<1, "no", "yes"),
         speed=Distance_cm/Time_sec,
        # Size_cat=case_when(Weight_g<0.04 ~"S",
         #                   Weight_g>0.04 ~"L"),
         temp_range=case_when(
           loc_temp >= 0   & loc_temp < 5    ~ "0-5",
           loc_temp >= 5   & loc_temp < 10   ~ "5-10",
           loc_temp >= 10  & loc_temp < 15   ~ "10-15",
           loc_temp >= 15  & loc_temp < 20   ~ "15-20",
           loc_temp >= 20  & loc_temp < 25   ~ "20-25",
           loc_temp >= 25  & loc_temp < 30   ~ "25-30",
           loc_temp >= 30  & loc_temp < 35   ~ "30-35",
           loc_temp >= 35  & loc_temp < 40   ~ "35-40",
           loc_temp >= 40  & loc_temp < 45   ~ "40-45",
           loc_temp >= 45  & loc_temp < 50   ~ "45-50",
           loc_temp >= 50  & loc_temp < 55   ~ "50-55",
           loc_temp >= 55  & loc_temp <= 60  ~ "55-60"))

write.csv(ecophys_dat, "ecophys_dat.csv")

#proceed to data_exploration.R file, if you want to visualize the data

#summary table for locomotion####

all_loc_dat=ecophys_dat%>%filter(!is.na(loc_temp))

loc_summary_dat=all_loc_dat%>%
  group_by(Location, Taxon, temp_range)%>%
  summarise(sample_size=n())

