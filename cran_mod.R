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

cran_dat=ecophys_dat%>%
  filter(Taxon=="craneflies",area=="high arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

cran_speed_dat=cran_dat %>%
  filter(movement == 1,!is.na(speed),!speed < 0)%>%
  mutate(ID_factor = factor(indiv_ID),ID_stan = as.integer(ID_factor),
         speed_scaled=speed/max(speed))%>%
  filter(!is.na(weight_z))

cran_temp_pred <- seq(
  min(cran_dat$loc_temp),
  max(cran_dat$loc_temp),
  length.out = 200)

cran_data_stan=list(
  N = nrow(cran_dat),
  temp = cran_dat$loc_temp,
  moved = cran_dat$movement,
  ID = cran_dat$indiv_ID,
  N_speed = nrow(cran_speed_dat),
  temp_speed = cran_speed_dat$loc_temp,
  speed = cran_speed_dat$speed,
  ID_speed = cran_speed_dat$indiv_ID,
  N_ID = length(unique(cran_dat$indiv_ID)),
  N_pred = length(cran_temp_pred),
  temp_pred = cran_temp_pred)

cran_movedata_stan=list(
  N = nrow(cran_dat),
  temp = cran_dat$loc_temp,
  moved = cran_dat$movement,
  ID = cran_dat$indiv_ID,
  N_speed = nrow(cran_speed_dat),
  temp_speed = cran_speed_dat$loc_temp,
  speed = cran_speed_dat$speed,
  ID_speed = cran_speed_dat$indiv_ID,
  N_ID = length(unique(cran_dat$indiv_ID)),
  N_pred = length(cran_temp_pred),
  temp_pred = cran_temp_pred)

cran_speeddata_stan=list(
  N = nrow(cran_speed_dat),
  temp = cran_speed_dat$loc_temp,
  speed = cran_speed_dat$speed_scaled,
  N_ID = nlevels(cran_speed_dat$ID_factor),
  ID = cran_speed_dat$ID_stan,
  N_pred = length(cran_temp_pred),
  weight=cran_speed_dat$weight_z,
  temp_pred = cran_temp_pred)

tpc_mod_cran=cmdstan_model("flex_tpc.stan") #speed+movement model
speed_mod=cmdstan_model("speed_mod.stan") #speed model
move_mod=cmdstan_model("movement_mod.stan") #movement model

fit_cran=tpc_mod_cran$sample(
                     data = cran_data_stan,
                     chains = 4,iter_warmup = 500,
                     iter_sampling = 2000,seed = 123,
                     adapt_delta = 0.95)
# 
# fit_cran$summary(
#   variables = c(
#     "pmax",
#     "Tmove",
#     "Tmin",
#     "Tmax",
#     "rmax",
#     "alpha",
#     "beta",
#     "sigma",
#     "sd_move",
#     "sd_speed"))

fit_move_cran=move_mod$sample(
  data = cran_movedata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_cran=speed_mod$sample(
  data = cran_speeddata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_cran$diagnostic_summary()
fit_move_cran$diagnostic_summary()

fit_speed_cran$summary(
  variables = c(
    "Topt",
    "Tmin",
    "Tmax",
    "alpha",
    "beta",
    "rmax",
    "cv",
    "b_weight"))

cran_movement_draws <- fit_cran$draws("movement_pred",format = "matrix")

cran_speed_draws <- fit_cran$draws("speed_pred",format = "matrix")

cran_overall_draws <- fit_cran$draws("overall_pred",format = "matrix")

cran_overall_obs=cran_dat%>%
  mutate(observed_performance = if_else(movement == 1,speed,0))

cran_overall_curve <- data.frame(
  temp = cran_temp_pred,
  mean = apply(cran_overall_draws,2,mean),
  lower = apply(cran_overall_draws,2,quantile,0.025),
  upper = apply(cran_overall_draws,2,quantile,0.975))

#SPEED
cran_speed_pars=fit_cran$draws(variables = c("Tmin", "Tmax", "alpha", "beta", "Topt"),
                               format = "data.frame")
cran_speed_Topt=(cran_speed_pars$alpha * cran_speed_pars$Tmax +
                   (1 - cran_speed_pars$alpha) * cran_speed_pars$Tmin)

quantile(cran_speed_Topt,probs = c(0.025, 0.5, 0.975)) 

#MOVEMENT
cran_movement_draws=fit_cran$draws(variables = "movement_pred",format = "matrix")

cran_movement_Topt=apply(cran_movement_draws,1,function(x) {cran_temp_pred[which.max(x)]})

quantile(cran_movement_Topt,probs = c(0.025, 0.5, 0.975))

#OVERALL
cran_overall_draws=fit_cran$draws(variables = "overall_pred",format = "matrix")

cran_overall_Topt=apply(cran_overall_draws,1,function(x) {cran_temp_pred[which.max(x)]})

quantile(cran_overall_Topt,probs = c(0.025, 0.5, 0.975))

#THERMAL LIMITS

# Posterior medians
cran_Topt_med=median(cran_overall_Topt)
cran_Tmin_med=median(cran_speed_pars$Tmin)
cran_Tmax_med=median(cran_speed_pars$Tmax)

#alpha
cran_alpha_med=median(cran_speed_pars$alpha)
quantile(cran_speed_pars$alpha, probs = c(0.025, 0.5, 0.975))

#beta
cran_beta_med=median(cran_speed_pars$beta)

n_draws_to_plot <- 100

cran_draw_ids <- sample(seq_len(nrow(cran_overall_draws)),n_draws_to_plot)

cran_overall_lines <- as.data.frame(cran_overall_draws[cran_draw_ids, , drop = FALSE]) %>%
  mutate(draw = seq_len(n_draws_to_plot)) %>%
  pivot_longer(cols = -draw,names_to = "temp_index",values_to = "overall_perf") %>%
  group_by(draw) %>%
  mutate(temp = cran_temp_pred) %>%
  ungroup()

ggplot() +geom_line(data = cran_overall_lines,aes(x = temp, y = overall_perf, group = draw),
                    alpha = 0.25,linewidth = 0.3) +
  geom_point(data = cran_overall_obs,aes(x = loc_temp, y = observed_performance),
    inherit.aes = FALSE,alpha = 0.4) +
  geom_line(data = cran_overall_curve,aes(x = temp, y = mean),linewidth = 1) +
  theme_classic() +
  labs(x = "Temperature (°C)", y = "overall performance") +
  ggtitle("craneflies (ZAC only)") +
  geom_vline(xintercept = cran_Topt_med, lty = 2, col = "black", lwd = 1) +
  geom_vline(xintercept = cran_Tmin_med, lty = 2, col = "grey", lwd = 1) +
  geom_vline(xintercept = cran_Tmax_med, lty = 2, col = "grey", lwd = 1) +
  annotate( "text", x = Inf, y = Inf,
            label = paste0( "\u03b1 = ", round(cran_alpha_med, 3),
      "\n\u03b2 = ", round(cran_beta_med, 3)), hjust = 2.5,vjust = 1.2)


##NEW MOD, SIMPLER

#MOVEMENT####
cran_movement_draws=fit_move_cran$draws(variables = "movement_pred",format = "matrix")

cran_movement_curve=summarise_curve(cran_movement_draws,cran_temp_pred)

cran_movement_lines=posterior_lines(cran_movement_draws,cran_temp_pred,n_draws = n_draw_lines)

cran_move_pars=fit_move_cran$draws(variables = c("pmax", "Tmove", "kcold", "khot"),
                                     format = "data.frame")

cran_movement_Topt=apply(cran_movement_draws,1, function(x) {cran_temp_pred[which.max(x)]})

cran_movement_Topt_quants=quantile(cran_movement_Topt,c(0.025,0.5,0.975))

#SPEED####
cran_speed_draws=fit_speed_cran$draws(variables = "speed_pred",format = "matrix")

cran_speed_draws_orig=cran_speed_draws*max(cran_speed_dat$speed)

cran_speed_curve=summarise_curve(cran_speed_draws,cran_temp_pred)

cran_speed_lines=posterior_lines(cran_speed_draws,cran_temp_pred,n_draws = n_draw_lines)

cran_speed_pars=fit_speed_cran$draws(variables=c("Topt","Tmin", "Tmax", "alpha", 
                                                   "beta", "rmax", "cv"),
                                       format = "data.frame")%>%
  mutate(rmax_orig=rmax*max(cran_speed_dat$speed))

#OVERALL PERFORMANCE

cran_n_joint= min(nrow(cran_movement_draws),nrow(cran_speed_draws_orig))

cran_move_ids <- sample(seq_len(nrow(cran_movement_draws)),cran_n_joint,replace = FALSE)

cran_speed_ids <- sample(seq_len(nrow(cran_speed_draws_orig)),cran_n_joint,replace = FALSE)

cran_movement_joint <-cran_movement_draws[cran_move_ids,,drop = FALSE]

cran_speed_joint <-cran_speed_draws_orig[cran_speed_ids,,drop = FALSE]

cran_overall_draws=cran_movement_joint*cran_speed_joint

cran_overall_curve<-summarise_curve(cran_overall_draws,cran_temp_pred)

cran_overall_lines<-posterior_lines(cran_overall_draws,cran_temp_pred,n_draws = n_draw_lines)

cran_overall_Topt <- apply(cran_overall_draws,1,function(x) {cran_temp_pred[which.max(x)]})
quantile(cran_overall_Topt, probs=c(0.025,0.5, 0.975))

# Posterior medians
cran_Topt_med=median(cran_overall_Topt)
cran_Tmin_med=median(cran_speed_pars$Tmin)
cran_Tmax_med=median(cran_speed_pars$Tmax)

#alpha
cran_alpha_med=median(cran_speed_pars$alpha)
quantile(cran_speed_pars$alpha, probs=c(0.025,0.5, 0.975))

#beta
cran_beta_med=median(cran_speed_pars$beta)
quantile(cran_speed_pars$beta, probs=c(0.025,0.5, 0.975))

cran_p_overall <- ggplot()+geom_line(data =cran_overall_lines,aes(x = temp,y = value,group = draw),
                                      alpha = 0.06,linewidth = 0.3) +
  geom_ribbon(data = cran_overall_curve,aes(x = temp,ymin = lower,ymax = upper),
              alpha = 0.2) +geom_line(data = cran_overall_curve,
                                      aes( x = temp,y = median),linewidth = 1.1)+
  geom_point(data =cran_dat,aes(x = loc_temp,y = if_else(movement == 0, 0, speed)), 
             alpha = 0.4) + theme_classic()+ggtitle("craneflies (ZAC only)")+
  geom_vline(xintercept=cran_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=cran_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=cran_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(cran_alpha_med, 3),
                           "\n\u03b2 = ", round(cran_beta_med, 3)),
            hjust = 2.5,vjust = 1.2)+labs(x = "Temperature (°C)",
                                          y = "overall performance")

cran_p_overall
