require(cmdstanr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)

source("C:\\pdumandanSLU\\PatD-SLU\\SLU\\TEMPNET\\2025\\Field-2025\\curve_functions.R")

filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\TEMPNET\\2025\\Field-2025"

filename = paste(filepath, '\\ecophys_dat','.csv', sep = '')

ecophys_dat=read.csv(filename,header=T, sep=",")

mus_dat_kobb=ecophys_dat%>%
  filter(Taxon=="muscids",area=="low arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

musk_speed_dat=mus_dat_kobb %>%
  filter(movement == 1,!is.na(speed),speed > 0)%>%
  mutate(ID_factor = factor(indiv_ID),ID_stan = as.integer(ID_factor),
         speed_scaled=speed/max(speed, na.rm=T))%>%
  filter(!is.na(weight_z))

musk_temp_pred <- seq(
  min(mus_dat_kobb$loc_temp),
  max(mus_dat_kobb$loc_temp),
  length.out = 200)

musk_movedata_stan=list(
  N = nrow(mus_dat_kobb),
  temp = mus_dat_kobb$loc_temp,
  moved = mus_dat_kobb$movement,
  ID = mus_dat_kobb$indiv_ID,
  N_speed = nrow(musk_speed_dat),
  temp_speed = musk_speed_dat$loc_temp,
  speed = musk_speed_dat$speed,
  ID_speed = musk_speed_dat$indiv_ID,
  N_ID = length(unique(mus_dat_kobb$indiv_ID)),
  N_pred = length(musk_temp_pred),
  temp_pred = musk_temp_pred)

musk_speeddata_stan=list(
  N = nrow(musk_speed_dat),
  temp = musk_speed_dat$loc_temp,
  speed = musk_speed_dat$speed_scaled,
  N_ID = nlevels(musk_speed_dat$ID_factor),
  ID = musk_speed_dat$ID_stan,
  N_pred = length(musk_temp_pred),
  weight=musk_speed_dat$weight_z,
  temp_pred = musk_temp_pred)

#tpc_mod_ws=cmdstan_model("flex_tpc.stan")
speed_mod=cmdstan_model("speed_mod.stan") #speed model
move_mod=cmdstan_model("movement_mod.stan") #movement model

fit_move_musk=move_mod$sample(
  data = musk_movedata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_musk=speed_mod$sample(
  data = musk_speeddata_stan, chains = 4,
  iter_warmup = 500,iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_musk$diagnostic_summary()
fit_move_musk$diagnostic_summary()

fit_speed_musk$summary(
  variables = c(
    "Topt",
    "Tmin",
    "Tmax",
    "alpha",
    "beta",
    "rmax",
    "cv",
    "b_weight"))

#MOVEMENT####
musk_movement_draws=fit_move_musk$draws(variables = "movement_pred",format = "matrix")

musk_movement_curve=summarise_curve(musk_movement_draws,musk_temp_pred)

musk_movement_lines=posterior_lines(musk_movement_draws,musk_temp_pred,n_draws = n_draw_lines)

musk_move_pars=fit_move_musk$draws(variables = c("pmax", "Tmove", "kcold", "khot"),
                                 format = "data.frame")

musk_movement_Topt=apply(musk_movement_draws,1, function(x) {musk_temp_pred[which.max(x)]})

musk_movement_Topt_quants=quantile(musk_movement_Topt,c(0.025,0.5,0.975))

#SPEED####
musk_speed_draws=fit_speed_musk$draws(variables = "speed_pred",format = "matrix")

musk_speed_draws_orig=musk_speed_draws*max(musk_speed_dat$speed)

musk_speed_curve=summarise_curve(musk_speed_draws,musk_temp_pred)

musk_speed_lines=posterior_lines(musk_speed_draws,musk_temp_pred,n_draws = n_draw_lines)

musk_speed_pars=fit_speed_musk$draws(variables=c("Topt","Tmin", "Tmax", "alpha", 
                                               "beta", "rmax", "cv"),
                                   format = "data.frame")%>%
  mutate(rmax_orig=rmax*max(ws_speed_dat$speed))

#OVERALL PERFORMANCE

musk_n_joint= min(nrow(musk_movement_draws),nrow(musk_speed_draws_orig))

musk_move_ids <- sample(seq_len(nrow(musk_movement_draws)),musk_n_joint,replace = FALSE)

musk_speed_ids <- sample(seq_len(nrow(musk_speed_draws_orig)),musk_n_joint,replace = FALSE)

musk_movement_joint <-musk_movement_draws[musk_move_ids,,drop = FALSE]

musk_speed_joint <-musk_speed_draws_orig[musk_speed_ids,,drop = FALSE]

musk_overall_draws=musk_movement_joint*musk_speed_joint

musk_overall_curve<-summarise_curve(musk_overall_draws,musk_temp_pred)

musk_overall_lines<-posterior_lines(musk_overall_draws,musk_temp_pred,n_draws = n_draw_lines)

musk_overall_Topt <- apply(musk_overall_draws,1,function(x) {musk_temp_pred[which.max(x)]})
quantile(musk_overall_Topt, probs=c(0.025,0.5, 0.975))

# Posterior medians
musk_Topt_med=median(musk_overall_Topt)
musk_Tmin_med=median(musk_speed_pars$Tmin)
musk_Tmax_med=median(musk_speed_pars$Tmax)

#alpha
musk_alpha_med=median(musk_speed_pars$alpha)
quantile(musk_speed_pars$alpha,c(0.025,0.5,0.975))

#beta
musk_beta_med=median(musk_speed_pars$beta)
quantile(musk_speed_pars$beta,c(0.025,0.5,0.975))

musk_p_overall <- ggplot()+geom_line(data =musk_overall_lines,aes(x = temp,y = value,group = draw),
                                    alpha = 0.06,linewidth = 0.3) +
  geom_ribbon(data = musk_overall_curve,aes(x = temp,ymin = lower,ymax = upper),
              alpha = 0.2) +geom_line(data = musk_overall_curve,
                                      aes( x = temp,y = median),linewidth = 1.1)+
  geom_point(data =mus_dat_kobb,aes(x = loc_temp,y = if_else(movement == 0, 0, speed)), 
             alpha = 0.4) + theme_classic()+ggtitle("muscids (Kobb only)")+
  geom_vline(xintercept=musk_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=musk_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=musk_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(musk_alpha_med, 3),
                           "\n\u03b2 = ", round(musk_beta_med, 3)),
            hjust = 2.5,vjust = 1.2)+labs(x = "Temperature (°C)",
                                          y = "overall performance")

musk_p_overall
