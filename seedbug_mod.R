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

seedb_dat=ecophys_dat%>%
  filter(Taxon=="seed_bug",area=="low arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

seedb_speed_dat=seedb_dat %>%
  filter(movement == 1,!is.na(speed),!speed < 0)%>%
  mutate(ID_factor = factor(indiv_ID),ID_stan = as.integer(ID_factor),
         speed_scaled=speed/max(speed))

seedb_temp_pred <- seq(
  min(seedb_dat$loc_temp),
  max(seedb_dat$loc_temp),
  length.out = 200)

seedb_movedata_stan=list(
  N = nrow(seedb_dat),
  temp = seedb_dat$loc_temp,
  moved = seedb_dat$movement,
  ID = seedb_dat$indiv_ID,
  N_speed = nrow(seedb_speed_dat),
  temp_speed = seedb_speed_dat$loc_temp,
  speed = seedb_speed_dat$speed,
  ID_speed = seedb_speed_dat$indiv_ID,
  N_ID = length(unique(seedb_dat$indiv_ID)),
  N_pred = length(seedb_temp_pred),
  temp_pred = seedb_temp_pred)

seedb_speeddata_stan=list(
  N = nrow(seedb_speed_dat),
  temp = seedb_speed_dat$loc_temp,
  speed = seedb_speed_dat$speed_scaled,
  N_ID = nlevels(seedb_speed_dat$ID_factor),
  ID = seedb_speed_dat$ID_stan,
  N_pred = length(seedb_temp_pred),
  temp_pred = seedb_temp_pred)

# tpc_mod_seedb=cmdstan_model("flex_tpc.stan")
# tpc_mod2_seedb=cmdstan_model("flex_tpc2.stan")
speed_mod_seedb=cmdstan_model("speed_mod.stan") #speed model
move_mod_seedb=cmdstan_model("movement_mod.stan") #movement model

# fit_seedb=tpc_mod2_seedb$sample(
#   data = seedb_data_stan,
#   chains = 4,
#   parallel_chains = 4,iter_warmup = 500,
#   iter_sampling = 2000,seed = 123,
#   adapt_delta = 0.95)

fit_move_seedb=move_mod_seedb$sample(
  data = seedb_movedata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_seedb=speed_mod_seedb$sample(
  data = seedb_speeddata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_seedb$diagnostic_summary()
fit_move_seedb$diagnostic_summary()

fit_speed_seedb$summary(
  variables = c(
    "Topt",
    "Tmin",
    "Tmax",
    "alpha",
    "beta",
    "rmax",
    "cv"))

#MOVEMENT####
seedb_movement_draws=fit_move_seedb$draws(variables = "movement_pred",format = "matrix")

seedb_movement_curve=summarise_curve(seedb_movement_draws,seedb_temp_pred)

seedb_movement_lines=posterior_lines(seedb_movement_draws,seedb_temp_pred,n_draws = n_draw_lines)

seedb_move_pars=fit_move_seedb$draws(variables = c("pmax", "Tmove", "kcold", "khot"),
                                 format = "data.frame")

seedb_movement_Topt=apply(seedb_movement_draws,1, function(x) {seedb_temp_pred[which.max(x)]})

seedb_movement_Topt_quants=quantile(seedb_movement_Topt,c(0.025,0.5,0.975))

#SPEED####
seedb_speed_draws=fit_speed_seedb$draws(variables = "speed_pred",format = "matrix")

seedb_speed_draws_orig=seedb_speed_draws*max(seedb_speed_dat$speed)

seedb_speed_curve=summarise_curve(seedb_speed_draws,seedb_temp_pred)

seedb_speed_lines=posterior_lines(seedb_speed_draws,seedb_temp_pred,n_draws = n_draw_lines)

seedb_speed_pars=fit_speed_seedb$draws(variables=c("Topt","Tmin", "Tmax", "alpha", 
                                               "beta", "rmax", "cv"),
                                   format = "data.frame")%>%
  mutate(rmax_orig=rmax*max(seedb_speed_dat$speed))

#OVERALL PERFORMANCE

seedb_n_joint= min(nrow(seedb_movement_draws),nrow(seedb_speed_draws_orig))

seedb_move_ids <- sample(seq_len(nrow(seedb_movement_draws)),seedb_n_joint,replace = FALSE)

seedb_speed_ids <- sample(seq_len(nrow(seedb_speed_draws_orig)),seedb_n_joint,replace = FALSE)

seedb_movement_joint <-seedb_movement_draws[seedb_move_ids,,drop = FALSE]

seedb_speed_joint <-seedb_speed_draws_orig[seedb_speed_ids,,drop = FALSE]

seedb_overall_draws=seedb_movement_joint*seedb_speed_joint

seedb_overall_curve<-summarise_curve(seedb_overall_draws,seedb_temp_pred)

seedb_overall_lines<-posterior_lines(seedb_overall_draws,seedb_temp_pred,n_draws = n_draw_lines)

seedb_overall_Topt <- apply(seedb_overall_draws,1,function(x) {seedb_temp_pred[which.max(x)]})

# Posterior medians
seedb_Topt_med=median(seedb_overall_Topt)
seedb_Tmin_med=median(seedb_speed_pars$Tmin)
seedb_Tmax_med=median(seedb_speed_pars$Tmax)

#alpha
seedb_alpha_med=median(seedb_speed_pars$alpha)

#beta
seedb_beta_med=median(seedb_speed_pars$beta)

seedb_p_overall <- ggplot()+geom_line(data =seedb_overall_lines,aes(x = temp,y = value,group = draw),
                                    alpha = 0.06,linewidth = 0.3) +
  geom_ribbon(data = seedb_overall_curve,aes(x = temp,ymin = lower,ymax = upper),
              alpha = 0.2) +geom_line(data = seedb_overall_curve,
                                      aes( x = temp,y = median),linewidth = 1.1)+
  geom_point(data =seedb_dat,aes(x = loc_temp,y = if_else(movement == 0, 0, speed)), 
             alpha = 0.4) + theme_classic()+ggtitle("seed bugs (Kobb only)")+
  geom_vline(xintercept=seedb_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=seedb_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=seedb_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(seedb_alpha_med, 3),
                           "\n\u03b2 = ", round(seedb_beta_med, 3)),
            hjust = 2.5,vjust = 1.2)+labs(x = "Temperature (°C)",
                                          y = "overall performance")

seedb_p_overall
