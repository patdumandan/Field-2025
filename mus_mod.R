require(cmdstanr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)

filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\TEMPNET\\2025\\Field-2025"

filename = paste(filepath, '\\ecophys_dat','.csv', sep = '')

ecophys_dat=read.csv(filename,header=T, sep=",")

mus_dat=ecophys_dat%>%
  filter(Taxon=="muscids",area=="high arctic",
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

mus_speed_dat=mus_dat %>%
  filter(movement == 1,!is.na(speed),!speed < 0)%>%
  mutate(ID_factor = factor(indiv_ID),ID_stan = as.integer(ID_factor),
         speed_scaled=speed/max(speed))

mus_temp_pred <- seq(
  min(mus_dat$loc_temp),
  max(mus_dat$loc_temp),
  length.out = 200)

mus_movedata_stan=list(
  N = nrow(mus_dat),
  temp = mus_dat$loc_temp,
  moved = mus_dat$movement,
  ID = mus_dat$indiv_ID,
  N_speed = nrow(mus_speed_dat),
  temp_speed = mus_speed_dat$loc_temp,
  speed = mus_speed_dat$speed,
  ID_speed = mus_speed_dat$indiv_ID,
  N_ID = length(unique(mus_dat$indiv_ID)),
  N_pred = length(mus_temp_pred),
  temp_pred = mus_temp_pred)

mus_speeddata_stan=list(
  N = nrow(mus_speed_dat),
  temp = mus_speed_dat$loc_temp,
  speed = mus_speed_dat$speed_scaled,
  N_ID = nlevels(mus_speed_dat$ID_factor),
  ID = mus_speed_dat$ID_stan,
  N_pred = length(mus_temp_pred),
  temp_pred = mus_temp_pred)

tpc_mod_mus=cmdstan_model("flex_tpc.stan")
speed_mod_mus=cmdstan_model("speed_mod.stan") #speed model
move_mod_mus=cmdstan_model("movement_mod.stan") #movement model

# fit_mus=tpc_mod_mus$sample(
#   data = mus_data_stan,
#   chains = 4,
#   parallel_chains = 4,iter_warmup = 500,
#   iter_sampling = 2000,seed = 123,
#   adapt_delta = 0.99)

fit_move_mus=move_mod_mus$sample(
  data = mus_movedata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_mus=speed_mod_mus$sample(
  data = mus_speeddata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_mus$diagnostic_summary()
fit_move_mus$diagnostic_summary()

fit_speed_mus$summary(
  variables = c(
    "Topt",
    "Tmin",
    "Tmax",
    "alpha",
    "beta",
    "rmax",
    "cv"))

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
mus_movement_draws=fit_move_mus$draws(variables = "movement_pred",format = "matrix")

mus_movement_curve=summarise_curve(mus_movement_draws,mus_temp_pred)

mus_movement_lines=posterior_lines(mus_movement_draws,mus_temp_pred,n_draws = n_draw_lines)

mus_move_pars=fit_move_mus$draws(variables = c("pmax", "Tmove", "kcold", "khot"),
                                 format = "data.frame")

mus_movement_Topt=apply(mus_movement_draws,1, function(x) {mus_temp_pred[which.max(x)]})

mus_movement_Topt_quants=quantile(mus_movement_Topt,c(0.025,0.5,0.975))

#SPEED####
mus_speed_draws=fit_speed_mus$draws(variables = "speed_pred",format = "matrix")

mus_speed_draws_orig=mus_speed_draws*max(mus_speed_dat$speed)

mus_speed_curve=summarise_curve(mus_speed_draws,mus_temp_pred)

mus_speed_lines=posterior_lines(mus_speed_draws,mus_temp_pred,n_draws = n_draw_lines)

mus_speed_pars=fit_speed_mus$draws(variables=c("Topt","Tmin", "Tmax", "alpha", 
                                               "beta", "rmax", "cv"),
                                   format = "data.frame")%>%
  mutate(rmax_orig=rmax*max(mus_speed_dat$speed))

#OVERALL PERFORMANCE

mus_n_joint= min(nrow(mus_movement_draws),nrow(mus_speed_draws_orig))

mus_move_ids <- sample(seq_len(nrow(mus_movement_draws)),mus_n_joint,replace = FALSE)

mus_speed_ids <- sample(seq_len(nrow(mus_speed_draws_orig)),mus_n_joint,replace = FALSE)

mus_movement_joint <-mus_movement_draws[mus_move_ids,,drop = FALSE]

mus_speed_joint <-mus_speed_draws_orig[mus_speed_ids,,drop = FALSE]

mus_overall_draws=mus_movement_joint*mus_speed_joint

mus_overall_curve<-summarise_curve(mus_overall_draws,mus_temp_pred)

mus_overall_lines<-posterior_lines(mus_overall_draws,mus_temp_pred,n_draws = n_draw_lines)

mus_overall_Topt <- apply(mus_overall_draws,1,function(x) {mus_temp_pred[which.max(x)]})

# Posterior medians
mus_Topt_med=median(mus_overall_Topt)
mus_Tmin_med=median(mus_speed_pars$Tmin)
mus_Tmax_med=median(mus_speed_pars$Tmax)

#alpha
mus_alpha_med=median(mus_speed_pars$alpha)

#beta
mus_beta_med=median(mus_speed_pars$beta)

mus_p_overall <- ggplot()+geom_line(data =mus_overall_lines,aes(x = temp,y = value,group = draw),
                                    alpha = 0.06,linewidth = 0.3) +
  geom_ribbon(data = mus_overall_curve,aes(x = temp,ymin = lower,ymax = upper),
              alpha = 0.2) +geom_line(data = mus_overall_curve,
                                      aes( x = temp,y = median),linewidth = 1.1)+
  geom_point(data =mus_dat,aes(x = loc_temp,y = if_else(movement == 0, 0, speed)), 
             alpha = 0.4) + theme_classic()+ggtitle("muscids (ZAC only)")+
  geom_vline(xintercept=mus_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=mus_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=mus_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(mus_alpha_med, 3),
                           "\n\u03b2 = ", round(mus_beta_med, 3)),
            hjust = 2.5,vjust = 1.2)+labs(x = "Temperature (°C)",
                                           y = "overall performance")

mus_p_overall
