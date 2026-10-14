require(cmdstanr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)

filepath = "C:\\pdumandanSLU\\PatD-SLU\\SLU\\TEMPNET\\2025\\Field-2025"

filename = paste(filepath, '\\ecophys_dat','.csv', sep = '')

ecophys_dat=read.csv(filename,header=T, sep=",")

mos_dat=ecophys_dat%>%
  filter(Taxon=="mosquito",area=="high arctic", year==2026,
         !is.na(loc_temp),
         !is.na(Distance_cm),
         !is.na(movement),
         !is.na(Time_sec))%>%
  mutate(indiv_ID=as.integer(as.factor(Sample_ID)))

mos_speed_dat=mos_dat %>%
  filter(movement == 1,!is.na(speed),!speed < 0)%>%
  mutate(ID_factor = factor(indiv_ID),ID_stan = as.integer(ID_factor),
         speed_scaled=speed/max(speed))%>%
  filter(!is.na(weight_z))

mos_temp_pred <- seq(
  min(mos_dat$loc_temp),
  max(mos_dat$loc_temp),
  length.out = 200)

mos_movedata_stan=list(
  N = nrow(mos_dat),
  temp = mos_dat$loc_temp,
  moved = mos_dat$movement,
  ID = mos_dat$indiv_ID,
  N_speed = nrow(mos_speed_dat),
  temp_speed = mos_speed_dat$loc_temp,
  speed = mos_speed_dat$speed,
  ID_speed = mos_speed_dat$indiv_ID,
  N_ID = length(unique(mos_dat$indiv_ID)),
  N_pred = length(mos_temp_pred),
  temp_pred = mos_temp_pred)

mos_speeddata_stan=list(
  N = nrow(mos_speed_dat),
  temp = mos_speed_dat$loc_temp,
  speed = mos_speed_dat$speed_scaled,
  N_ID = nlevels(mos_speed_dat$ID_factor),
  ID = mos_speed_dat$ID_stan,
  N_pred = length(mos_temp_pred),
  weight=mos_speed_dat$weight_z,
  temp_pred = mos_temp_pred)

#tpc_mod_mos=cmdstan_model("flex_tpc.stan") #speed+movement model
speed_mod=cmdstan_model("speed_mod.stan") #speed model
move_mod=cmdstan_model("movement_mod.stan") #movement model

fit_mos=tpc_mod_mos$sample(
  data = mos_data_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_move_mos=move_mod$sample(
  data = mos_movedata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_mos=speed_mod$sample(
  data = mos_speeddata_stan,
  chains = 4,iter_warmup = 500,
  iter_sampling = 2000,seed = 123,
  adapt_delta = 0.95)

fit_speed_mos$diagnostic_summary()
fit_move_mos$diagnostic_summary()

fit_speed_mos$summary(
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
mos_movement_draws=fit_move_mos$draws(variables = "movement_pred",format = "matrix")

mos_movement_curve=summarise_curve(mos_movement_draws,mos_temp_pred)

mos_movement_lines=posterior_lines(mos_movement_draws,mos_temp_pred,n_draws = n_draw_lines)

mos_move_pars=fit_move_mos$draws(variables = c("pmax", "Tmove", "kcold", "khot"),
                                 format = "data.frame")

mos_movement_Topt=apply(mos_movement_draws,1, function(x) {mos_temp_pred[which.max(x)]})

mos_movement_Topt_quants=quantile(mos_movement_Topt,c(0.025,0.5,0.975))

#SPEED####
mos_speed_draws=fit_speed_mos$draws(variables = "speed_pred",format = "matrix")

mos_speed_draws_orig=mos_speed_draws*max(mos_speed_dat$speed)

mos_speed_curve=summarise_curve(mos_speed_draws,mos_temp_pred)

mos_speed_lines=posterior_lines(mos_speed_draws,mos_temp_pred,n_draws = n_draw_lines)

mos_speed_pars=fit_speed_mos$draws(variables=c("Topt","Tmin", "Tmax", "alpha", 
                                               "beta", "rmax", "cv"),
                                   format = "data.frame")%>%
  mutate(rmax_orig=rmax*max(mos_speed_dat$speed))

#OVERALL PERFORMANCE

set.seed(123)

mos_n_joint= min(nrow(mos_movement_draws),nrow(mos_speed_draws_orig))

mos_move_ids <- sample(seq_len(nrow(mos_movement_draws)),mos_n_joint,replace = FALSE)

mos_speed_ids <- sample(seq_len(nrow(mos_speed_draws_orig)),mos_n_joint,replace = FALSE)

mos_movement_joint <-mos_movement_draws[mos_move_ids,,drop = FALSE]

mos_speed_joint <-mos_speed_draws_orig[mos_speed_ids,,drop = FALSE]

mos_overall_draws=mos_movement_joint*mos_speed_joint

mos_overall_curve<-summarise_curve(mos_overall_draws,mos_temp_pred)

mos_overall_lines<-posterior_lines(mos_overall_draws,mos_temp_pred,n_draws = n_draw_lines)

mos_overall_Topt <- apply(mos_overall_draws,1,function(x) {mos_temp_pred[which.max(x)]})
quantile(mos_overall_Topt,c(0.025,0.5,0.975))

# Posterior medians
mos_Topt_med=median(mos_overall_Topt)
mos_Tmin_med=median(mos_speed_pars$Tmin)
mos_Tmax_med=median(mos_speed_pars$Tmax)

#alpha
mos_alpha_med=median(mos_speed_pars$alpha)
quantile(mos_speed_pars$alpha,c(0.025,0.5,0.975))

#beta
mos_beta_med=median(mos_speed_pars$beta)
quantile(mos_speed_pars$beta,c(0.025,0.5,0.975))

mos_p_overall <- ggplot()+geom_line(data =mos_overall_lines,aes(x = temp,y = value,group = draw),
                                    alpha = 0.06,linewidth = 0.3) +
  geom_ribbon(data = mos_overall_curve,aes(x = temp,ymin = lower,ymax = upper),
              alpha = 0.2) +geom_line(data = mos_overall_curve,
                                      aes( x = temp,y = median),linewidth = 1.1)+
  geom_point(data =mos_dat,aes(x = loc_temp,y = if_else(movement == 0, 0, speed)), 
             alpha = 0.4) + theme_classic()+ggtitle("mosquitoes (ZAC only)")+
  geom_vline(xintercept=mos_Topt_med, lty=2, col="black", lwd=1)+
  geom_vline(xintercept=mos_Tmin_med, lty=2, col="grey", lwd=1)+
  geom_vline(xintercept=mos_Tmax_med, lty=2, col="grey", lwd=1)+
  annotate( "text", x = Inf,y = Inf,
            label = paste0("\u03b1 = ", round(mos_alpha_med, 3),
                           "\n\u03b2 = ", round(mos_beta_med, 3)),
            hjust = 2.5,vjust = 1.2)

mos_p_overall
