require(cmdstanr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)

filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\TEMPNET\\2025\\Field-2025"

filename = paste(filepath, '\\ecophys_dat','.csv', sep = '')

ecophys_dat=read.csv(filename,header=T, sep=",")

weev_dat=ecophys_dat%>%
  filter(Taxon=="weevil",area=="low arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

weev_speed_dat=weev_dat %>%
  filter(movement == 1,!is.na(speed),!speed < 0)%>%
  mutate(ID_factor = factor(indiv_ID),ID_stan = as.integer(ID_factor),
         speed_scaled=speed/max(speed))%>%
  filter(!is.na(weight_z))

weev_temp_pred <- seq(
  min(weev_dat$loc_temp),
  max(weev_dat$loc_temp),
  length.out = 200)

weev_movedata_stan=list(
  N = nrow(weev_dat),
  temp = weev_dat$loc_temp,
  moved = weev_dat$movement,
  ID = weev_dat$indiv_ID,
  N_speed = nrow(weev_speed_dat),
  temp_speed = weev_speed_dat$loc_temp,
  speed = weev_speed_dat$speed,
  ID_speed = weev_speed_dat$indiv_ID,
  N_ID = length(unique(weev_dat$indiv_ID)),
  N_pred = length(weev_temp_pred),
  temp_pred = weev_temp_pred)

weev_speeddata_stan=list(
  N = nrow(weev_speed_dat),
  temp = weev_speed_dat$loc_temp,
  speed = weev_speed_dat$speed_scaled,
  N_ID = nlevels(weev_speed_dat$ID_factor),
  ID = weev_speed_dat$ID_stan,
  N_pred = length(weev_temp_pred),
  weight=weev_speed_dat$weight_z,
  temp_pred = weev_temp_pred)

tpc_mod_weev=cmdstan_model("flex_tpc.stan")
speed_mod_weev=cmdstan_model("speed_mod.stan") #speed model
move_mod_weev=cmdstan_model("movement_mod.stan") #movement model

fit_move_weev=move_mod_weev$sample(
  data = weev_movedata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_weev=speed_mod_weev$sample(
  data = weev_speeddata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_weev$diagnostic_summary()
fit_move_weev$diagnostic_summary()

fit_speed_weev$summary(
  variables = c(
    "Topt",
    "Tmin",
    "Tmax",
    "alpha",
    "beta",
    "rmax",
    "cv",
    "b_weight"))

n_draw_lines <- 100
set.seed(123)

summarise_curve = function(draw_matrix, temp_pred) {
  data.frame(
    temp = temp_pred,
    median = apply(draw_matrix,2,median),
    mean = apply(draw_matrix,2,mean),
    lower = apply(draw_matrix,2,quantile,probs = 0.025),
    upper = apply(draw_matrix,2,quantile,probs = 0.975))}

posterior_lines= function(draw_matrix,temp_pred,n_draws = 100) {
  n_draws <- min(n_draws,nrow(draw_matrix))
  selected <- sample(seq_len(nrow(draw_matrix)),n_draws)
  as.data.frame(draw_matrix[selected, , drop = FALSE])%>%
    mutate(draw = seq_len(n_draws))%>%
    pivot_longer(cols = -draw,names_to = "prediction",values_to = "value" )%>%
    group_by(draw)%>%mutate(temp = temp_pred) %>%ungroup()}

#MOVEMENT####
weev_movement_draws=fit_move_weev$draws(variables = "movement_pred",format = "matrix")

weev_movement_curve=summarise_curve(weev_movement_draws,weev_temp_pred)

weev_movement_lines=posterior_lines(weev_movement_draws,weev_temp_pred,n_draws = n_draw_lines)

weev_move_pars=fit_move_weev$draws(variables = c("pmax", "Tmove", "kcold", "khot"),
                                 format = "data.frame")

weev_movement_Topt=apply(weev_movement_draws,1, function(x) {weev_temp_pred[which.max(x)]})

weev_movement_Topt_quants=quantile(weev_movement_Topt,c(0.025,0.5,0.975))

#SPEED####
weev_speed_draws=fit_speed_weev$draws(variables = "speed_pred",format = "matrix")

weev_speed_draws_orig=weev_speed_draws*max(weev_speed_dat$speed)

weev_speed_curve=summarise_curve(weev_speed_draws,weev_temp_pred)

weev_speed_lines=posterior_lines(weev_speed_draws,weev_temp_pred,n_draws = n_draw_lines)

weev_speed_pars=fit_speed_weev$draws(variables=c("Topt","Tmin", "Tmax", "alpha", 
                                               "beta", "rmax", "cv"),
                                   format = "data.frame")%>%
  mutate(rmax_orig=rmax*max(weev_speed_dat$speed))

#OVERALL PERFORMANCE

weev_n_joint= min(nrow(weev_movement_draws),nrow(weev_speed_draws_orig))

weev_move_ids <- sample(seq_len(nrow(weev_movement_draws)),weev_n_joint,replace = FALSE)

weev_speed_ids <- sample(seq_len(nrow(weev_speed_draws_orig)),weev_n_joint,replace = FALSE)

weev_movement_joint <-weev_movement_draws[weev_move_ids,,drop = FALSE]

weev_speed_joint <-weev_speed_draws_orig[weev_speed_ids,,drop = FALSE]

weev_overall_draws=weev_movement_joint*weev_speed_joint

weev_overall_curve<-summarise_curve(weev_overall_draws,weev_temp_pred)

weev_overall_lines<-posterior_lines(weev_overall_draws,weev_temp_pred,n_draws = n_draw_lines)

weev_overall_Topt <- apply(weev_overall_draws,1,function(x) {weev_temp_pred[which.max(x)]})
quantile(weev_overall_Topt, probs=c(0.025, 0.5, 0.975))

# Posterior medians
weev_Topt_med=median(weev_overall_Topt)
weev_Tmin_med=median(weev_speed_pars$Tmin)
weev_Tmax_med=median(weev_speed_pars$Tmax)

#alpha
weev_alpha_med=median(weev_speed_pars$alpha)
quantile(weev_speed_pars$alpha, probs=c(0.025, 0.5, 0.975))

#beta
weev_beta_med=median(weev_speed_pars$beta)
quantile(weev_speed_pars$beta, probs=c(0.025, 0.5, 0.975))

weev_p_overall <- ggplot()+geom_line(data =weev_overall_lines,aes(x = temp,y = value,group = draw),
                                    alpha = 0.06,linewidth = 0.3) +
  geom_ribbon(data = weev_overall_curve,aes(x = temp,ymin = lower,ymax = upper),
              alpha = 0.2) +geom_line(data = weev_overall_curve,
                                      aes( x = temp,y = median),linewidth = 1.1)+
  geom_point(data =weev_dat,aes(x = loc_temp,y = if_else(movement == 0, 0, speed)), 
             alpha = 0.4) + theme_classic()+ggtitle("weevils (Kobb only)")+
  geom_vline(xintercept=weev_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=weev_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=weev_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(weev_alpha_med, 3),
                           "\n\u03b2 = ", round(weev_beta_med, 3)),
            hjust = 2.5,vjust = 1.2)+labs(x = "Temperature (°C)",
                                          y = "overall performance")
weev_p_overall
