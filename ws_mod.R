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

ws_dat=ecophys_dat%>%
  filter(Taxon=="wolf_spider",area=="high arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

ws_speed_dat=ws_dat %>%
  filter(movement == 1,!is.na(speed),speed > 0)%>%
  mutate(ID_factor = factor(indiv_ID),ID_stan = as.integer(ID_factor),
         speed_scaled=speed/max(speed))

ws_temp_pred <- seq(
  min(ws_dat$loc_temp),
  max(ws_dat$loc_temp),
  length.out = 200)

ws_movedata_stan=list(
  N = nrow(ws_dat),
  temp = ws_dat$loc_temp,
  moved = ws_dat$movement,
  ID = ws_dat$indiv_ID,
  N_speed = nrow(ws_speed_dat),
  temp_speed = ws_speed_dat$loc_temp,
  speed = ws_speed_dat$speed,
  ID_speed = ws_speed_dat$indiv_ID,
  N_ID = length(unique(ws_dat$indiv_ID)),
  N_pred = length(ws_temp_pred),
  temp_pred = ws_temp_pred)

ws_speeddata_stan=list(
  N = nrow(ws_speed_dat),
  temp = ws_speed_dat$loc_temp,
  speed = ws_speed_dat$speed_scaled,
  N_ID = nlevels(ws_speed_dat$ID_factor),
  ID = ws_speed_dat$ID_stan,
  N_pred = length(ws_temp_pred),
  temp_pred = ws_temp_pred)

#tpc_mod_ws=cmdstan_model("flex_tpc.stan")
speed_mod_ws=cmdstan_model("speed_mod.stan") #speed model
move_mod_ws=cmdstan_model("movement_mod.stan") #movement model

fit_move_ws=move_mod_ws$sample(
  data = ws_movedata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_ws=speed_mod_ws$sample(
  data = ws_speeddata_stan, chains = 4,
  iter_warmup = 500,iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_ws$diagnostic_summary()
fit_move_ws$diagnostic_summary()

fit_speed_ws$summary(
  variables = c(
    "Topt",
    "Tmin",
    "Tmax",
    "alpha",
    "beta",
    "rmax",
    "cv"))

#MOVEMENT####
ws_movement_draws=fit_move_ws$draws(variables = "movement_pred",format = "matrix")

ws_movement_curve=summarise_curve(ws_movement_draws,ws_temp_pred)

ws_movement_lines=posterior_lines(ws_movement_draws,ws_temp_pred,n_draws = n_draw_lines)

ws_move_pars=fit_move_ws$draws(variables = c("pmax", "Tmove", "kcold", "khot"),
                                     format = "data.frame")

ws_movement_Topt=apply(ws_movement_draws,1, function(x) {ws_temp_pred[which.max(x)]})

ws_movement_Topt_quants=quantile(ws_movement_Topt,c(0.025,0.5,0.975))

#SPEED####
ws_speed_draws=fit_speed_ws$draws(variables = "speed_pred",format = "matrix")

ws_speed_draws_orig=ws_speed_draws*max(ws_speed_dat$speed)

ws_speed_curve=summarise_curve(ws_speed_draws,ws_temp_pred)

ws_speed_lines=posterior_lines(ws_speed_draws,ws_temp_pred,n_draws = n_draw_lines)

ws_speed_pars=fit_speed_ws$draws(variables=c("Topt","Tmin", "Tmax", "alpha", 
                                                   "beta", "rmax", "cv"),
                                       format = "data.frame")%>%
  mutate(rmax_orig=rmax*max(ws_speed_dat$speed))

#OVERALL PERFORMANCE

ws_n_joint= min(nrow(ws_movement_draws),nrow(ws_speed_draws_orig))

ws_move_ids <- sample(seq_len(nrow(ws_movement_draws)),ws_n_joint,replace = FALSE)

ws_speed_ids <- sample(seq_len(nrow(ws_speed_draws_orig)),ws_n_joint,replace = FALSE)

ws_movement_joint <-ws_movement_draws[ws_move_ids,,drop = FALSE]

ws_speed_joint <-ws_speed_draws_orig[ws_speed_ids,,drop = FALSE]

ws_overall_draws=ws_movement_joint*ws_speed_joint

ws_overall_curve<-summarise_curve(ws_overall_draws,ws_temp_pred)

ws_overall_lines<-posterior_lines(ws_overall_draws,ws_temp_pred,n_draws = n_draw_lines)

ws_overall_Topt <- apply(ws_overall_draws,1,function(x) {ws_temp_pred[which.max(x)]})

# Posterior medians
ws_Topt_med=median(ws_overall_Topt)
ws_Tmin_med=median(ws_speed_pars$Tmin)
ws_Tmax_med=median(ws_speed_pars$Tmax)

#alpha
ws_alpha_med=median(ws_speed_pars$alpha)

#beta
ws_beta_med=median(ws_speed_pars$beta)

ws_p_overall <- ggplot()+geom_line(data =ws_overall_lines,aes(x = temp,y = value,group = draw),
                                      alpha = 0.06,linewidth = 0.3) +
  geom_ribbon(data = ws_overall_curve,aes(x = temp,ymin = lower,ymax = upper),
              alpha = 0.2) +geom_line(data = ws_overall_curve,
                                      aes( x = temp,y = median),linewidth = 1.1)+
  geom_point(data =ws_dat,aes(x = loc_temp,y = if_else(movement == 0, 0, speed)), 
             alpha = 0.4) + theme_classic()+ggtitle("wolf spiders (ZAC only)")+
  geom_vline(xintercept=ws_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=ws_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=ws_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(ws_alpha_med, 3),
                           "\n\u03b2 = ", round(ws_beta_med, 3)),
            hjust = 2.5,vjust = 1.2)+labs(x = "Temperature (°C)",
                                          y = "overall performance")

ws_p_overall
