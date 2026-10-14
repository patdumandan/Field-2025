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

emp_dat_kobb=ecophys_dat%>%
  filter(Taxon=="empids",area=="low arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

empk_speed_dat=emp_dat_kobb %>%
  filter(movement == 1,!is.na(speed),speed > 0)%>%
  mutate(ID_factor = factor(indiv_ID),ID_stan = as.integer(ID_factor),
         speed_scaled=speed/max(speed, na.rm=T))%>%
  filter(!is.na(weight_z))

empk_temp_pred <- seq(
  min(emp_dat_kobb$loc_temp),
  max(emp_dat_kobb$loc_temp),
  length.out = 200)

empk_movedata_stan=list(
  N = nrow(emp_dat_kobb),
  temp = emp_dat_kobb$loc_temp,
  moved = emp_dat_kobb$movement,
  ID = emp_dat_kobb$indiv_ID,
  N_speed = nrow(empk_speed_dat),
  temp_speed = empk_speed_dat$loc_temp,
  speed = empk_speed_dat$speed,
  ID_speed = empk_speed_dat$indiv_ID,
  N_ID = length(unique(emp_dat_kobb$indiv_ID)),
  N_pred = length(empk_temp_pred),
  temp_pred = empk_temp_pred)

empk_speeddata_stan=list(
  N = nrow(empk_speed_dat),
  temp = empk_speed_dat$loc_temp,
  speed = empk_speed_dat$speed_scaled,
  N_ID = nlevels(empk_speed_dat$ID_factor),
  ID = empk_speed_dat$ID_stan,
  N_pred = length(empk_temp_pred),
  weight=empk_speed_dat$weight_z,
  temp_pred = empk_temp_pred)

#tpc_mod_ws=cmdstan_model("flex_tpc.stan")
speed_mod=cmdstan_model("speed_mod.stan") #speed model
move_mod=cmdstan_model("movement_mod.stan") #movement model

fit_move_empk=move_mod$sample(
  data = empk_movedata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_empk=speed_mod$sample(
  data = empk_speeddata_stan, chains = 4,
  iter_warmup = 500,iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_empk$diagnostic_summary()
fit_move_empk$diagnostic_summary()

fit_speed_empk$summary(
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
empk_movement_draws=fit_move_empk$draws(variables = "movement_pred",format = "matrix")

empk_movement_curve=summarise_curve(empk_movement_draws,empk_temp_pred)

empk_movement_lines=posterior_lines(empk_movement_draws,empk_temp_pred,n_draws = n_draw_lines)

empk_move_pars=fit_move_empk$draws(variables = c("pmax", "Tmove", "kcold", "khot"),
                                 format = "data.frame")

empk_movement_Topt=apply(empk_movement_draws,1, function(x) {empk_temp_pred[which.max(x)]})

empk_movement_Topt_quants=quantile(empk_movement_Topt,c(0.025,0.5,0.975))

#SPEED####
empk_speed_draws=fit_speed_empk$draws(variables = "speed_pred",format = "matrix")

empk_speed_draws_orig=empk_speed_draws*max(empk_speed_dat$speed)

empk_speed_curve=summarise_curve(empk_speed_draws,empk_temp_pred)

empk_speed_lines=posterior_lines(empk_speed_draws,empk_temp_pred,n_draws = n_draw_lines)

empk_speed_pars=fit_speed_empk$draws(variables=c("Topt","Tmin", "Tmax", "alpha", 
                                               "beta", "rmax", "cv"),
                                   format = "data.frame")%>%
  mutate(rmax_orig=rmax*max(ws_speed_dat$speed))

#OVERALL PERFORMANCE

empk_n_joint= min(nrow(empk_movement_draws),nrow(empk_speed_draws_orig))

empk_move_ids <- sample(seq_len(nrow(empk_movement_draws)),empk_n_joint,replace = FALSE)

empk_speed_ids <- sample(seq_len(nrow(empk_speed_draws_orig)),empk_n_joint,replace = FALSE)

empk_movement_joint <-empk_movement_draws[empk_move_ids,,drop = FALSE]

empk_speed_joint <-empk_speed_draws_orig[empk_speed_ids,,drop = FALSE]

empk_overall_draws=empk_movement_joint*empk_speed_joint

empk_overall_curve<-summarise_curve(empk_overall_draws,empk_temp_pred)

empk_overall_lines<-posterior_lines(empk_overall_draws,empk_temp_pred,n_draws = n_draw_lines)

empk_overall_Topt <- apply(empk_overall_draws,1,function(x) {empk_temp_pred[which.max(x)]})
quantile(empk_overall_Topt, probs=c(0.025,0.5, 0.975))

# Posterior medians
empk_Topt_med=median(empk_overall_Topt)
empk_Tmin_med=median(empk_speed_pars$Tmin)
empk_Tmax_med=median(empk_speed_pars$Tmax)

#alpha
empk_alpha_med=median(empk_speed_pars$alpha)
quantile(empk_speed_pars$alpha,c(0.025,0.5,0.975))

#beta
empk_beta_med=median(empk_speed_pars$beta)
quantile(empk_speed_pars$beta,c(0.025,0.5,0.975))

empk_p_overall <- ggplot()+geom_line(data =empk_overall_lines,aes(x = temp,y = value,group = draw),
                                    alpha = 0.06,linewidth = 0.3) +
  geom_ribbon(data = empk_overall_curve,aes(x = temp,ymin = lower,ymax = upper),
              alpha = 0.2) +geom_line(data = empk_overall_curve,
                                      aes( x = temp,y = median),linewidth = 1.1)+
  geom_point(data =emp_dat_kobb,aes(x = loc_temp,y = if_else(movement == 0, 0, speed)), 
             alpha = 0.4) + theme_classic()+ggtitle("empidids (Kobb only)")+
  geom_vline(xintercept=empk_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=empk_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=empk_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(empk_alpha_med, 3),
                           "\n\u03b2 = ", round(empk_beta_med, 3)),
            hjust = 2.5,vjust = 1.2)+labs(x = "Temperature (°C)",
                                          y = "overall performance")

empk_p_overall
