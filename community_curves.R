#community performance curves

taxon_cols <- c(
  "craneflies"    = "#E69F00",
  "mosquitoes"    = "#56B4E9",
  "muscids"       = "#009E73",
  "empidids"      = "#CC79A7",
  "wolf_spiders"  = "#D55E00",
  "seed_bugs"     = "#0072B2",
  "weevils"       = "#F0E442"
)

#lines
cran_speed_lines=cran_speed_lines%>%mutate(taxon="craneflies")
mos_speed_lines=mos_speed_lines%>%mutate(taxon="mosquitoes")
mus_speed_lines=mus_speed_lines%>%mutate(taxon="muscids")
emp_speed_lines=emp_speed_lines%>%mutate(taxon="empidids")
ws_speed_lines=ws_speed_lines%>%mutate(taxon="wolf_spiders")

zac_line=bind_rows(cran_speed_lines, mos_speed_lines, mus_speed_lines, 
                   emp_speed_lines,ws_speed_lines)

cran_overall_curve=cran_overall_curve%>%mutate(taxon="craneflies")
mos_overall_curve=mos_overall_curve%>%mutate(taxon="mosquitoes")
mus_overall_curve=mus_overall_curve%>%mutate(taxon="muscids")
emp_overall_curve=emp_overall_curve%>%mutate(taxon="empidids")
ws_overall_curve=ws_overall_curve%>%mutate(taxon="wolf_spiders")

zac_curve=bind_rows(cran_overall_curve, mos_overall_curve, mus_overall_curve, 
                    emp_overall_curve,ws_overall_curve)

zac_lines_relative= zac_line%>%group_by(taxon,draw)%>%
  mutate(relative_value =value/max(value)) %>%ungroup()

zac_curves_relative= zac_curve%>%group_by(taxon) %>%
  mutate(relative_value =median/max(median)) %>%ungroup()
zac_comm=ggplot()+geom_line(data = zac_lines_relative, aes(x = temp,y = relative_value,
                                                  group = interaction(taxon, draw),colour = taxon),
                   alpha = 0.08,linewidth = 0.25)+geom_line(
                     data = zac_curves_relative,aes(x = temp,y = relative_value,colour = taxon),
                     linewidth = 1.2)+theme_classic()+scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Temperature (°C)",y = "Relative overall performance",
       colour = "Taxon",title = "TPCs (Zackenberg)")+scale_colour_manual(values=taxon_cols)

#community performance curves

#lines
seedb_speed_lines=seedb_speed_lines%>%mutate(taxon="seed_bugs")
weev_speed_lines=weev_speed_lines%>%mutate(taxon="weevils")
musk_speed_lines=musk_speed_lines%>%mutate(taxon="muscids")
empk_speed_lines=empk_speed_lines%>%mutate(taxon="empidids")
wsk_speed_lines=wsk_speed_lines%>%mutate(taxon="wolf_spiders")

kobb_line=bind_rows(seedb_speed_lines, weev_speed_lines, musk_speed_lines, 
                    empk_speed_lines,wsk_speed_lines)

seedb_overall_curve=seedb_overall_curve%>%mutate(taxon="seed_bugs")
weev_overall_curve=weev_overall_curve%>%mutate(taxon="weevils")
musk_overall_curve=musk_overall_curve%>%mutate(taxon="muscids")
empk_overall_curve=empk_overall_curve%>%mutate(taxon="empidids")
wsk_overall_curve=wsk_overall_curve%>%mutate(taxon="wolf_spiders")

kobb_curve=bind_rows(seedb_overall_curve, weev_overall_curve, musk_overall_curve, 
                     empk_overall_curve,wsk_overall_curve)

kobb_lines_relative= kobb_line%>%group_by(taxon,draw)%>%
  mutate(relative_value =value/max(value)) %>%ungroup()

kobb_curves_relative= kobb_curve%>%group_by(taxon) %>%
  mutate(relative_value =median/max(median)) %>%ungroup()

kobb_comm=ggplot()+geom_line(data = kobb_lines_relative, aes(x = temp,y = relative_value,
                                                             group = interaction(taxon, draw),colour = taxon),
                             alpha = 0.08,linewidth = 0.25)+geom_line(
                               data = kobb_curves_relative,aes(x = temp,y = relative_value,colour = taxon),
                               linewidth = 1.2)+theme_classic()+scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Temperature (°C)",y = "Relative overall performance",
       colour = "Taxon",title = "TPCs (Kobbefjord)")+scale_colour_manual(values=taxon_cols)

ggarrange(zac_comm, kobb_comm)
