// hurdle model for fitting TPCs
//with 2 submodels for 1) P(move|T) and 2) E(speed|movement, T)
functions {
real p_move(
      real temp, //temp at obs i
      real pmax, //max prob of movement
      real Tcold, //temp describing low-temp transition
      real Thot,//temp describing high temp transition
      real kcold, //controls how rapidly movement prob. increases around Tcold
      real khot //controls how rapidly movement prob. decreases around Thot
  ) {

    real cold_response=inv_logit(kcold*(temp-Tcold));
    real hot_response=inv_logit(khot*(Thot-temp));
    
    return pmax * cold_response * hot_response;
  }

//using flexTPC
//note:
// alpha close to 0= Topt closer to Tmin,
//alpha close to 1= Topt closer to Tmax
//larger beta=broad TPC
//smaller beta=narrow/sharper peaked TPC

    real flex_tpc(
      real temp, //temp at obs i
      real Tmin, //lower thermal limit
      real Tmax,//upper thermal limit
      real rmax,//max predicted speed
      real alpha, //controls where Topt occurs; Topt = alpha * Tmax + (1 - alpha) * Tmin
      real beta //controls breadth of curve
  ) {

    // if outside the thermal limits, performance = 0
    if (temp <= Tmin || temp >= Tmax)
      return 0;

    //shape param (uses alpha and beta)
    real s = alpha * (1 - alpha) / square(beta);

    return rmax*exp(s*(alpha * log((temp - Tmin) / alpha)+
        (1 - alpha) * log((Tmax - temp) / (1 - alpha))
        -log(Tmax - Tmin)));
  }
}
//probability of movement~temp
//note:

//large khot/kcold=abrupt transition to cold/decline at high temps
//small khot=gradual transition/decline
//units of change: 1/degC

data {

  // Observed data
  int<lower=1> N;
  vector[N] temp;
  array[N] int<lower=0, upper=1> moved;
  array[N] int<lower=1> ID; //sample ID

  int<lower=1> N_speed;
  vector[N_speed] temp_speed;
  vector<lower=0>[N_speed] speed;
  array[N_speed] int<lower=1> ID_speed;

  // Individuals
  int<lower=1> N_ID;
  
  // Temperature grid for posterior curves
  int<lower=1> N_pred;
  vector[N_pred] temp_pred;
}


parameters {

//movement prams
  real<lower=0, upper=1> pmax;
  ordered[2] Tmove;
  real<lower=0> kcold;
  real<lower=0> khot;

 // indiv. variation in movement tendency
  vector[N_ID] z_move;
  real<lower=0> sd_move;
  
//speed TPC params
  real<upper=min(temp_speed)> Tmin;
  real<lower=max(temp_speed)> Tmax;
  real<lower=0> rmax;
  real<lower=0, upper=1> alpha;
  real<lower=0> beta;

  real<lower=0> sigma; //SD around speed curve
  
  vector[N_ID] z_speed;  // Individual differences in speed
  real<lower=0> sd_speed;
}

model {

//movement params
  pmax ~ beta(2, 1);
  Tmove[1] ~ normal(min(temp), 10);
  Tmove[2] ~ normal(max(temp), 10);

  kcold ~ lognormal(log(0.3), 0.8);
  khot  ~ lognormal(log(0.3), 0.8);

//speed params
  Tmin ~ normal(min(temp_speed)-2.5, 5); //or min-5
  Tmax ~ normal(max(temp_speed)+ 2.5, 5);//or max-5

  rmax ~ normal(0, 10);

  alpha ~ beta(2,2); //or beta(1, 1)

  beta ~ gamma(square(0.25)/square(0.12),0.25/square(0.12));//same as mcruzloyola

  sigma ~ normal(0, 5);

  // Random effects; sample ID
  z_move ~ normal(0, 1);
  z_speed ~ normal(0, 1);

  sd_move ~ normal(0, 1);
  sd_speed ~ normal(0, 0.5);

  // --------------------------------------------------
  // MOVEMENT MODEL
  // --------------------------------------------------

   for (i in 1:N) {

    real p = p_move(temp[i],pmax,
                    Tmove[1],Tmove[2],
                    kcold,khot);

    // individual random effect on logit scale
    real p_ind = inv_logit(logit(p)+sd_move * z_move[ID[i]]);

    moved[i] ~ bernoulli(p_ind);
  }

  // --------------------------------------------------
  // SPEED MODEL
  // --------------------------------------------------
 for (i in 1:N_speed) {

    real mu = flex_tpc(temp_speed[i],Tmin,Tmax,
                       rmax,alpha,beta);

    // Allow some individuals to be generally faster/slower
    mu *= exp(sd_speed * z_speed[ID_speed[i]]);

    // Gamma distribution written using mean and SD
    real shape = square(mu) / square(sigma);

    real rate = mu / square(sigma);

    speed[i] ~ gamma(shape,rate);
  }
}

generated quantities {
  
  // Predicted curves
  vector[N_pred] movement_pred;
  vector[N_pred] speed_pred;
  vector[N_pred] overall_pred;
  real Topt;
  
  //Topt estimate
  Topt=alpha * Tmax+(1 - alpha) * Tmin;

  for (i in 1:N_pred) {

    // Probability of movement
    movement_pred[i]=p_move(temp_pred[i],pmax,
                            Tmove[1],Tmove[2],
                            kcold,khot);
                            
    // Expected speed if indiv moves
    speed_pred[i]=flex_tpc(temp_pred[i],Tmin,Tmax,
                              rmax,alpha,beta);


    // Overall locomotor performance
    overall_pred[i] =movement_pred[i]*speed_pred[i];
   
  }
}

