// MOVEMENT MODEL
//P(move|temp)
functions {

  // temp:temperature at observation i
  // pmax: upper ceiling for probability of movement
  // Tcold: midpoint of the low-temperature transition
  // Thot:midpoint of the high-temperature transition
  // kcold: steepness of increase around Tcold
  // khot: steepness of decline around Thot
  // Large k = more abrupt transition; Small k = more gradual transition
  
  real p_move(
      real temp,
      real pmax,
      real Tcold,
      real Thot,
      real kcold,
      real khot) {

  real cold_response=inv_logit(kcold * (temp - Tcold));

  real hot_response =inv_logit(khot * (Thot - temp));

    // Overall probability of movement
    return pmax *cold_response *hot_response;}
}


data {

  int<lower=1> N;
  vector[N] temp;
  array[N] int<lower=0, upper=1> moved; // 0 = did not move >= 1 cm
 // int<lower=1> N_ID;
 // array[N]int<lower=1, upper=N_ID> ID;
  int<lower=1> N_pred;
  vector[N_pred] temp_pred;
}

parameters {
  real<lower=0, upper=1> pmax;   // Upper ceiling for probability of movement
  // Tmove[1] = cold-side transition; Tmove[2] = hot-side transition
  // ordered[] guarantees: Tmove[1] < Tmove[2]
  ordered[2] Tmove;
  real<lower=0> kcold;
  real<lower=0> khot;

 // vector[N_ID] z_move;
 // real<lower=0> sd_move;
}

model {
  pmax ~ beta(2,1);
  Tmove[1] ~ normal(min(temp),10);
  Tmove[2] ~ normal(max(temp),10);

  kcold ~ lognormal(log(0.3),0.8);
  khot ~ lognormal(log(0.3),0.8);

 // z_move ~ normal(0,1);
 // sd_move ~ normal(0,1);

  for (i in 1:N) {

  real p=p_move(temp[i],pmax,Tmove[1],Tmove[2],kcold,khot);

 // real p_ind =inv_logit(logit(p));

  moved[i] ~ bernoulli(p);}
}

generated quantities {
  
  vector[N_pred] movement_pred;

  for (i in 1:N_pred) {

    movement_pred[i]=p_move(temp_pred[i],pmax,Tmove[1],Tmove[2],kcold,khot);
  }
}

