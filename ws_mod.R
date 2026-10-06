ws_dat=ecophys_dat%>%
  filter(Taxon=="wolf_spider",area=="high arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

ws_speed_dat=ws_dat %>%
  filter(movement == 1,!is.na(speed),speed > 0)

ws_temp_pred <- seq(
  min(ws_dat$loc_temp),
  max(ws_dat$loc_temp),
  length.out = 200)

ws_data_stan=list(
  N = nrow(ws_dat),
  temp = ws_dat$loc_temp,
  moved = ws_dat$movement,
  ID = ws_dat$indiv_ID,
  N_speed = nrow(ws_speed_dat),
  temp_speed = ws_speed_dat$loc_temp,
  speed = ws_speed_dat$speed,
  ID_speed = ws_speed_dat$indiv_ID,
  N_ID = length(unique(ws_dat$indiv_ID)),
  N_pred = length(temp_pred),
  temp_pred = temp_pred)

tpc_mod_ws=cmdstan_model("flex_tpc.stan")

fit_ws=tpc_mod_ws$sample(data = ws_data_stan,
                             chains = 4,
                             parallel_chains = 4,iter_warmup = 500,
                             iter_sampling = 2000,seed = 123,
                             adapt_delta = 0.95)

fit_ws$summary(
  variables = c(
    "pmax",
    "Tmove",
    "Tmin",
    "Tmax",
    "Topt",
    "rmax",
    "alpha",
    "beta",
    "sigma",
    "sd_move",
    "sd_speed"))

ws_movement_draws <- fit_ws$draws("movement_pred",format = "matrix")

ws_speed_draws <- fit_ws$draws("speed_pred",format = "matrix")

ws_overall_draws <- fit_ws$draws("overall_pred",format = "matrix")

ws_overall_obs=ws_dat%>%
  mutate(observed_performance = if_else(movement == 1,speed,0))

ws_overall_curve <- data.frame(
  temp = ws_temp_pred,
  mean = apply(ws_overall_draws,2,mean),
  lower = apply(ws_overall_draws,2,quantile,0.025),
  upper = apply(ws_overall_draws,2,quantile,0.975))

ggplot(ws_overall_curve,aes(x = temp,y = mean)) +
  geom_point(data = ws_overall_obs,aes(
    x = loc_temp,y = observed_performance),
    inherit.aes = FALSE,alpha = 0.4) +
  geom_ribbon(aes(ymin = lower,ymax = upper),alpha = 0.2) +
  geom_line(linewidth = 1) +
  theme_classic() +
  labs(x = "Temperature (°C)",
       y = "overall locomotor performance")+
  ggtitle("wolf spiders (ZAC only)")+
  geom_vline(xintercept=ws_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=ws_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=ws_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(ws_alpha_med, 2),
                           "\n\u03b2 = ", round(ws_beta_med, 2)),
            hjust = 3.5,vjust = 1.2)

#SPEED
ws_speed_pars=fit_ws$draws(variables = c("Tmin", "Tmax", "alpha", "beta", "Topt"),
                               format = "data.frame")
ws_speed_Topt=(ws_speed_pars$alpha * ws_speed_pars$Tmax +
                   (1 - ws_speed_pars$alpha) * ws_speed_pars$Tmin)

quantile(ws_speed_Topt,probs = c(0.025, 0.5, 0.975)) #mean Topt=25.49C

#MOVEMENT
ws_movement_draws=fit_ws$draws(variables = "movement_pred",format = "matrix")

ws_movement_Topt=apply(ws_movement_draws,1,function(x) {temp_pred[which.max(x)]})

quantile(ws_movement_Topt,probs = c(0.025, 0.5, 0.975)) #mean Topt=37.90C

#OVERALL
ws_overall_draws=fit_ws$draws(variables = "overall_pred",format = "matrix")

ws_overall_Topt=apply(ws_overall_draws,1,function(x) {temp_pred[which.max(x)]})

quantile(ws_overall_Topt,probs = c(0.025, 0.5, 0.975)) #mean Topt=25.69C

#THERMAL LIMITS

# Posterior medians
ws_Tmin_med=median(ws_speed_pars$Tmin)
ws_Tmax_med=median(ws_speed_pars$Tmax)

#alpha
ws_alpha_med=median(ws_speed_pars$alpha)

#alpha
ws_beta_med=median(ws_speed_pars$beta)

#topt
ws_Topt_med=median(ws_speed_pars$Topt)
