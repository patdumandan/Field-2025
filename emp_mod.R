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

emp_dat=ecophys_dat%>%
  filter(Taxon=="empids",area=="high arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

emp_speed_dat=emp_dat %>%
  filter(movement == 1,!is.na(speed),!speed < 0)%>%
  mutate(ID_factor = factor(indiv_ID),ID_stan = as.integer(ID_factor),
         speed_scaled=speed/max(speed))%>%
  filter(!is.na(weight_z))

emp_temp_pred <- seq(
  min(emp_dat$loc_temp),
  max(emp_dat$loc_temp),
  length.out = 200)

emp_movedata_stan=list(
  N = nrow(emp_dat),
  temp = emp_dat$loc_temp,
  moved = emp_dat$movement,
  ID = emp_dat$indiv_ID,
  N_speed = nrow(emp_speed_dat),
  temp_speed = emp_speed_dat$loc_temp,
  speed = emp_speed_dat$speed,
  ID_speed = emp_speed_dat$indiv_ID,
  N_ID = length(unique(emp_dat$indiv_ID)),
  N_pred = length(emp_temp_pred),
  temp_pred = emp_temp_pred)

emp_speeddata_stan=list(
  N = nrow(emp_speed_dat),
  temp = emp_speed_dat$loc_temp,
  speed = emp_speed_dat$speed_scaled,
  N_ID = nlevels(emp_speed_dat$ID_factor),
  ID = emp_speed_dat$ID_stan,
  N_pred = length(emp_temp_pred),
  weight = emp_speed_dat$weight_z,
  temp_pred = emp_temp_pred)

#tpc_mod_emp=cmdstan_model("flex_tpc.stan")
speed_mod=cmdstan_model("speed_mod.stan") #speed model
move_mod=cmdstan_model("movement_mod.stan") #movement model

fit_move_emp=move_mod$sample(
  data = emp_movedata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_emp=speed_mod$sample(
  data = emp_speeddata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_emp$diagnostic_summary()
fit_move_emp$diagnostic_summary()

fit_speed_emp$summary(
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
emp_movement_draws=fit_move_emp$draws(variables = "movement_pred",format = "matrix")

emp_movement_curve=summarise_curve(emp_movement_draws,emp_temp_pred)

emp_movement_lines=posterior_lines(emp_movement_draws,emp_temp_pred,n_draws = n_draw_lines)

emp_move_pars=fit_move_emp$draws(variables = c("pmax", "Tmove", "kcold", "khot"),
                                     format = "data.frame")

emp_movement_Topt=apply(emp_movement_draws,1, function(x) {emp_temp_pred[which.max(x)]})

emp_movement_Topt_quants=quantile(emp_movement_Topt,c(0.025,0.5,0.975))

#SPEED####
emp_speed_draws=fit_speed_emp$draws(variables = "speed_pred",format = "matrix")

emp_speed_draws_orig=emp_speed_draws*max(emp_speed_dat$speed)

emp_speed_curve=summarise_curve(emp_speed_draws,emp_temp_pred)

emp_speed_lines=posterior_lines(emp_speed_draws,emp_temp_pred,n_draws = n_draw_lines)

emp_speed_pars=fit_speed_emp$draws(variables=c("Topt","Tmin", "Tmax", "alpha", 
                                                   "beta", "rmax", "cv"),
                                       format = "data.frame")%>%
  mutate(rmax_orig=rmax*max(emp_speed_dat$speed))

#OVERALL PERFORMANCE

emp_n_joint= min(nrow(emp_movement_draws),nrow(emp_speed_draws_orig))

emp_move_ids <- sample(seq_len(nrow(emp_movement_draws)),emp_n_joint,replace = FALSE)

emp_speed_ids <- sample(seq_len(nrow(emp_speed_draws_orig)),emp_n_joint,replace = FALSE)

emp_movement_joint <-emp_movement_draws[emp_move_ids,,drop = FALSE]

emp_speed_joint <-emp_speed_draws_orig[emp_speed_ids,,drop = FALSE]

emp_overall_draws=emp_movement_joint*emp_speed_joint

emp_overall_curve<-summarise_curve(emp_overall_draws,emp_temp_pred)

emp_overall_lines<-posterior_lines(emp_overall_draws,emp_temp_pred,n_draws = n_draw_lines)

emp_overall_Topt <- apply(emp_overall_draws,1,function(x) {emp_temp_pred[which.max(x)]})
quantile(emp_overall_Topt, probs=c(0.025, 0.5, 0.975))

# Posterior medians
emp_Topt_med=median(emp_overall_Topt)
emp_Tmin_med=median(emp_speed_pars$Tmin)
emp_Tmax_med=median(emp_speed_pars$Tmax)

#alpha
emp_alpha_med=median(emp_speed_pars$alpha)
quantile(emp_speed_pars$alpha, probs=c(0.025, 0.5, 0.975))

#beta
emp_beta_med=median(emp_speed_pars$beta)
quantile(emp_speed_pars$beta, probs=c(0.025, 0.5, 0.975))

emp_p_overall <- ggplot()+geom_line(data =emp_overall_lines,aes(x = temp,y = value,group = draw),
                                      alpha = 0.06,linewidth = 0.3) +
  geom_ribbon(data = emp_overall_curve,aes(x = temp,ymin = lower,ymax = upper),
              alpha = 0.2) +geom_line(data = emp_overall_curve,
                                      aes( x = temp,y = median),linewidth = 1.1)+
  geom_point(data =emp_dat,aes(x = loc_temp,y = if_else(movement == 0, 0, speed)), 
             alpha = 0.4) + theme_classic()+ggtitle("empidids (ZAC only)")+
  geom_vline(xintercept=emp_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=emp_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=emp_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(emp_alpha_med, 3),
                           "\n\u03b2 = ", round(emp_beta_med, 3)),
            hjust = 2.5,vjust = 1.2)+labs(x = "Temperature (°C)",
                                          y = "overall performance")

emp_p_overall
