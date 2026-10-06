require(cmdstanr)

emp_dat=ecophys_dat%>%
  filter(Taxon=="empids",area=="high arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

emp_speed_dat=emp_dat %>%
  filter(movement == 1,!is.na(speed),!speed < 0)

temp_pred <- seq(
  min(emp_dat$loc_temp),
  max(emp_dat$loc_temp),
  length.out = 200)

emp_data_stan=list(
  N = nrow(emp_dat),
  temp = emp_dat$loc_temp,
  moved = emp_dat$movement,
  ID = emp_dat$indiv_ID,
  N_speed = nrow(emp_speed_dat),
  temp_speed = emp_speed_dat$loc_temp,
  speed = emp_speed_dat$speed,
  ID_speed = emp_speed_dat$indiv_ID,
  N_ID = length(unique(emp_dat$indiv_ID)),
  N_pred = length(temp_pred),
  temp_pred = temp_pred)

tpc_mod_emp=cmdstan_model("flex_tpc.stan")

fit_emp=tpc_mod_emp$sample(
  data = emp_data_stan,
  chains = 4,
  parallel_chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_emp$summary(
  variables = c(
    "pmax",
    "Tmove",
    "Topt",
    "Tmin",
    "Tmax",
    "rmax",
    "alpha",
    "beta",
    "sigma",
    "sd_move",
    "sd_speed"))

emp_movement_draws <- fit_emp$draws("movement_pred",format = "matrix")

emp_speed_draws <- fit_emp$draws("speed_pred",format = "matrix")

emp_overall_draws <- fit_emp$draws("overall_pred",format = "matrix")

emp_overall_obs=emp_dat%>%
  mutate(observed_performance = if_else(movement == 1,speed,0))

emp_overall_curve <- data.frame(
  temp = temp_pred,
  mean = apply(emp_overall_draws,2,mean),
  lower = apply(emp_overall_draws,2,quantile,0.025),
  upper = apply(emp_overall_draws,2,quantile,0.975))

#SPEED
emp_speed_pars=fit_emp$draws(variables = c("Tmin", "Tmax", "alpha", "beta", "Topt"),
                               format = "data.frame")
emp_speed_Topt=(emp_speed_pars$alpha * emp_speed_pars$Tmax +
                   (1 - emp_speed_pars$alpha) * emp_speed_pars$Tmin)

quantile(emp_speed_Topt,probs = c(0.025, 0.5, 0.975)) #mean Topt=25.49C

#MOVEMENT
emp_movement_draws=fit_emp$draws(variables = "movement_pred",format = "matrix")

emp_movement_Topt=apply(emp_movement_draws,1,function(x) {temp_pred[which.max(x)]})

quantile(emp_movement_Topt,probs = c(0.025, 0.5, 0.975)) #mean Topt=37.90C

#OVERALL
emp_overall_draws=fit_emp$draws(variables = "overall_pred",format = "matrix")

emp_overall_Topt=apply(emp_overall_draws,1,function(x) {temp_pred[which.max(x)]})

quantile(emp_overall_Topt,probs = c(0.025, 0.5, 0.975)) #mean Topt=25.69C

#THERMAL LIMITS

# Posterior medians
emp_Topt_med=median(emp_overall_Topt)
emp_Tmin_med=median(emp_speed_pars$Tmin)
emp_Tmax_med=median(emp_speed_pars$Tmax)

#alpha
emp_alpha_med=median(emp_speed_pars$alpha)

#beta
emp_beta_med=median(emp_speed_pars$beta)

ggplot(emp_overall_curve,aes(x = temp,y = mean)) +
  geom_point(data = emp_overall_obs,aes(
    x = loc_temp,y = observed_performance),
    inherit.aes = FALSE,alpha = 0.4) +
  geom_ribbon(aes(ymin = lower,ymax = upper),alpha = 0.2) +
  geom_line(linewidth = 1) +
  theme_classic() +
  labs(x = "Temperature (°C)",
       y = "overall locomotor performance")+
  ggtitle("empids (ZAC only)")+
  geom_vline(xintercept=emp_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=emp_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=emp_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(emp_alpha_med, 3),
                           "\n\u03b2 = ", round(emp_beta_med, 3)),
            hjust = 2.5,vjust = 1.2)

