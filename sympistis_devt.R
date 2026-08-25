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
  mutate(weight_change1=log(weight_2/weight_1)/2,
         weight_change2=log(weight_3/weight_2)/2,
         weight_change3=log(weight_4/weight_3)/2,
         weight_change4=log(weight_5/weight_4)/2,
         weight_change5=log(weight_6/weight_5)/2,
         weight_change6=log(weight_7/weight_6)/2,
         weight_change7=log(weight_8/weight_7)/2,
         weight_change8=log(weight_9/weight_8)/2,
         weight_change9=log(weight_10/weight_9)/2,
         weight_change10=log(weight_11/weight_10)/2,
         weight_change11=log(weight_12/weight_11)/2,
         start_weight1  = weight_1,
         start_weight2  = weight_2,
         start_weight3  = weight_3,
         start_weight4  = weight_4,
         start_weight5  = weight_5,
         start_weight6  = weight_6,
         start_weight7  = weight_7,
         start_weight8  = weight_8,
         start_weight9  = weight_9,
         start_weight10 = weight_10,
         start_weight11 = weight_11)%>%
  mutate(temperature=case_when(House==9 & Cold.Warm== "C" ~ "22",#changed to 22
                               House==9 & Cold.Warm== "W" ~ "34",
                               House==3 & Cold.Warm== "W" ~ "18",#18
                               House==3 & Cold.Warm== "C" ~ "7",
                               House==11 & Cold.Warm== "C" ~ "18",
                               House==11 & Cold.Warm== "W" ~ "27", #changed to 40
                               House==2 & Cold.Warm== "C" ~ "9",
                               House==2 & Cold.Warm== "W" ~ "29"))

#use for model 2 w/c includes start_weight
sym_dat_rgr=sym_dat%>%
  pivot_longer(cols = matches("^(weight_change|start_weight)\\d+$"),
               names_to = c(".value", "period"),
               names_pattern = "(weight_change|start_weight)(\\d+)")%>%
  mutate(period = as.integer(period))

#make temp changes
sym_dat$Temp_new=ifelse(is.na(sym_dat$Temp_04Jul), sym_dat$temperature, sym_dat$Temp_04Jul)
#temp changes made on 04-Jul

sym_dat$temperature=as.integer(as.character(sym_dat$temperature))
sym_dat$Temp_new =as.integer(as.character(sym_dat$Temp_new))

sym_dat1=sym_dat%>%filter(!is.na(Cold.Warm))

#use for mod1 and mod3
sym_dat_wc=sym_dat1%>%
  select(-stage_1,-stage_2,-stage_3, -stage_4,-stage_5,
         -stage_6,-stage_7,-stage_8,-stage_9,-stage_10, -stage_11,-stage_12,
         -weight_1,-weight_2,-weight_3, -weight_4,-weight_5,
         -weight_6,-weight_7,-weight_8,-weight_9,-weight_10, -weight_11,-weight_12,
         -Temp_04Jul, -temp_change)%>%
  pivot_longer(cols=starts_with("weight_change"), names_to = "period", values_to = "ROC")%>%
  group_by(House, Cold.Warm, Temp_new, ID)%>%
  mutate(time=seq(1:11))%>%
  ungroup()

sym_dat_wc$ID=as.factor(sym_dat_wc$ID)

#models
rgr_mod1=lmer(ROC~Temp_new+I(Temp_new^2) +I(Temp_new^3)+ (1|ID), data=sym_dat_wc)
rgr_mod2=lmer(weight_change~start_weight+Temp_new+I(Temp_new^2) +I(Temp_new^3)+ (1|ID), data=sym_dat_rgr)
rgr_mod3=lmer(ROC~Temp_new+I(Temp_new^2)+ (1|ID), data=sym_dat_wc)

pred_data=data.frame(Temp_new = seq(min(sym_dat_wc$Temp_new, na.rm = TRUE),
                                    max(sym_dat_wc$Temp_new, na.rm = TRUE),
                                    length.out = 200))

pred_data2=data.frame(Temp_new = seq(min(sym_dat_rgr$Temp_new, na.rm = TRUE),
                                     max(sym_dat_rgr$Temp_new, na.rm = TRUE),
                                     length.out = 200),
                      start_weight = mean(sym_dat_rgr$start_weight, na.rm = TRUE))

pred_data3=data.frame(Temp_new = seq(min(sym_dat_wc$Temp_new, na.rm = TRUE),
                                     max(sym_dat_wc$Temp_new, na.rm = TRUE),
                                     length.out = 200))

pred_data$predicted=predict(rgr_mod1,newdata = pred_data, re.form = NA) # re.form = NA removes individual-specific random effects; for plotting popn curve
pred_data2$predicted=predict(rgr_mod2,newdata = pred_data2, re.form = NA) # re.form = NA removes individual-specific random effects; for plotting popn curve
pred_data3$predicted=predict(rgr_mod3,newdata = pred_data3, re.form = NA) # re.form = NA removes individual-specific random effects; for plotting popn curve

X=model.matrix(~ Temp_new + I(Temp_new^2)+I(Temp_new^3),data = pred_data)

X2=model.matrix(~ start_weight+Temp_new + I(Temp_new^2)+I(Temp_new^3),data = pred_data2)

X3=model.matrix(~ Temp_new + I(Temp_new^2),data = pred_data3)

# Variance-covariance matrix of fixed effects
V=vcov(rgr_mod1)
V2=vcov(rgr_mod2)
V3=vcov(rgr_mod3)

# Standard error of predicted mean
pred_data$SE=sqrt(diag(X %*% V %*% t(X)))
pred_data2$SE=sqrt(diag(X2 %*% V2 %*% t(X2)))
pred_data3$SE=sqrt(diag(X3 %*% V3 %*% t(X3)))

# 95% confidence intervals
pred_data$lower=pred_data$predicted - 1.96 * pred_data$SE
pred_data$upper=pred_data$predicted + 1.96 * pred_data$SE

pred_data2$lower=pred_data2$predicted - 1.96 * pred_data2$SE
pred_data2$upper=pred_data2$predicted + 1.96 * pred_data2$SE

pred_data3$lower=pred_data3$predicted - 1.96 * pred_data3$SE
pred_data3$upper=pred_data3$predicted + 1.96 * pred_data3$SE

g1=ggplot(sym_dat_wc,aes(x = Temp_new, y = ROC))+
  geom_point(alpha = 0.35)+
  geom_line(data = pred_data,aes(x = Temp_new,y = predicted),alpha = 0.15, linewidth=1)+
  theme_classic()+
  labs(x = "Temperature (°C)",y = "Relative growth",title = "Temp_new+I(Temp_new^2) +I(Temp_new^3)+ (1|ID)")+
  geom_ribbon(data = pred_data,aes(x = Temp_new,ymin = lower,ymax = upper),
              inherit.aes = FALSE,alpha = 0.2)

gtemps=c(7, 9, 18, 22, 29, 34, 40)

g2=ggplot(sym_dat_rgr,aes(x = Temp_new, y = weight_change)) +
  geom_ribbon(data = pred_data2,aes(x = Temp_new,ymin = lower,ymax = upper),
              inherit.aes = FALSE,alpha = 0.2,fill = "red")+
  geom_line(data = pred_data2,aes(x = Temp_new,y = predicted),
            inherit.aes = FALSE,linewidth = 1,col = "red") +
  annotate("segment",x = temps,xend = temps,y = -0.14,yend = -0.105,
           arrow = arrow(length = unit(0.12, "cm"),type = "closed"),linewidth = 0.4)+
  scale_x_continuous(breaks = seq(5, 40, by = 5)) +
  coord_cartesian(ylim = c(-0.1, 0.25),xlim = c(5, 42),clip = "off") +
  theme_classic()+
  theme(plot.margin = margin(t = 5,r = 5,b = 30,l = 5)) +
  labs(x = "Temperature (°C)",y = "Relative growth")

g3=ggplot(sym_dat_wc,aes(x = Temp_new, y = ROC))+
  geom_point(alpha = 0.35)+
  geom_line(data = pred_data3,aes(x = Temp_new,y = predicted),alpha = 0.15, linewidth=1)+
  theme_classic()+
  labs(x = "Temperature (°C)",y = "Relative growth",title = "Temp_new+I(Temp_new^2)")+
  geom_ribbon(data = pred_data3,aes(x = Temp_new,ymin = lower,ymax = upper),
              inherit.aes = FALSE,alpha = 0.2)

ggarrange(g1,g2, g3)

#sanity check
ggplot(sym_dat_wc,aes(ROC))+
  geom_histogram()+theme_classic()+
  geom_vline(xintercept=0, lty=2, col="grey", lwd=1)

