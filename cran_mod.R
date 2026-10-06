require(cmdstanr)

cran_dat=ecophys_dat%>%
  filter(Taxon=="craneflies",area=="high arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

cran_speed_dat=cran_dat %>%
  filter(movement == 1,!is.na(speed),!speed < 0)

temp_pred <- seq(
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
  N_pred = length(temp_pred),
  temp_pred = temp_pred)

tpc_mod_cran=cmdstan_model("flex_tpc.stan")

fit_cran=tpc_mod_cran$sample(
                     data = cran_data_stan,
                     chains = 4,
                     parallel_chains = 4,iter_warmup = 500,
                     iter_sampling = 2000,seed = 123,
                     adapt_delta = 0.95)

fit_cran$summary(
  variables = c(
    "pmax",
    "Tmove",
    "Tmin",
    "Tmax",
    "rmax",
    "alpha",
    "beta",
    "sigma",
    "sd_move",
    "sd_speed"))

cran_movement_draws <- fit_cran$draws("movement_pred",format = "matrix")

cran_speed_draws <- fit_cran$draws("speed_pred",format = "matrix")

cran_overall_draws <- fit_cran$draws("overall_pred",format = "matrix")

cran_overall_obs=cran_dat%>%
  mutate(observed_performance = if_else(movement == 1,speed,0))

cran_overall_curve <- data.frame(
  temp = temp_pred,
  mean = apply(cran_overall_draws,2,mean),
  lower = apply(cran_overall_draws,2,quantile,0.025),
  upper = apply(cran_overall_draws,2,quantile,0.975))

#SPEED
cran_speed_pars=fit_cran$draws(variables = c("Tmin", "Tmax", "alpha", "beta"),
                               format = "data.frame")
cran_speed_Topt=(cran_speed_pars$alpha * cran_speed_pars$Tmax +
                   (1 - cran_speed_pars$alpha) * cran_speed_pars$Tmin)

quantile(cran_speed_Topt,probs = c(0.025, 0.5, 0.975)) #mean Topt=25.49C

#MOVEMENT
cran_movement_draws=fit_cran$draws(variables = "movement_pred",format = "matrix")

cran_movement_Topt=apply(cran_movement_draws,1,function(x) {temp_pred[which.max(x)]})

quantile(cran_movement_Topt,probs = c(0.025, 0.5, 0.975)) #mean Topt=37.90C

#OVERALL
cran_overall_draws=fit_cran$draws(variables = "overall_pred",format = "matrix")

cran_overall_Topt=apply(cran_overall_draws,1,function(x) {temp_pred[which.max(x)]})

quantile(cran_overall_Topt,probs = c(0.025, 0.5, 0.975)) #mean Topt=25.69C

#THERMAL LIMITS

# Posterior medians
cran_Tmin_med=median(cran_speed_pars$Tmin)
cran_Tmax_med=median(cran_speed_pars$Tmax)

#alpha
cran_alpha_med=median(cran_speed_pars$alpha)

#beta
cran_beta_med=median(cran_speed_pars$beta)

ggplot(cran_overall_curve,aes(x = temp,y = mean)) +
  geom_point(data = cran_overall_obs,aes(
    x = loc_temp,y = observed_performance),
    inherit.aes = FALSE,alpha = 0.4) +
  geom_ribbon(aes(ymin = lower,ymax = upper),alpha = 0.2) +
  geom_line(linewidth = 1) +
  theme_classic() +
  labs(x = "Temperature (°C)",
       y = "overall locomotor performance")+
  ggtitle("craneflies (ZAC only)")+
  geom_vline(xintercept=25.69, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=cran_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=cran_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
              label = paste0("\u03b1 = ", round(cran_alpha_med, 3),
                             "\n\u03b2 = ", round(cran_beta_med, 3)),
              hjust = 2.5,vjust = 1.2)

