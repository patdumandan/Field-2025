// SPEED TPC with indiv variation accounted for
// model: E(speed | movement, temperature)
// Only positive speeds from individuals that moved

functions {
  // temp:temperature at observation i
  // Tmin:lower thermal limit
  // Tmax:upper thermal limit
  // rmax:maximum expected speed for a typical individual
  // alpha:determines location of Topt between Tmin and Tmax
  // beta:controls thermal breadth
  // low beta = narrow curve, high beta= broad curve
  
  real flex_tpc(
      real temp,
      real Tmin,
      real Tmax,
      real rmax,
      real alpha,
      real beta) {

    // assume that performance is zero outside estimated thermal limits
    if (temp <= Tmin || temp >= Tmax) return 0;

    real s=alpha*(1 - alpha) /square(beta); //shape param

    return rmax*exp(s*(alpha*log((temp - Tmin)/alpha)+
          (1 - alpha) *log((Tmax - temp)/(1 - alpha))-
          log(Tmax - Tmin)));
  }
}

data {
  int<lower=1> N;
  vector[N] temp;
  vector<lower=0>[N] speed;
  vector [N] weight;
 // int<lower=1> N_ID;
//  array[N]int<lower=1, upper=N_ID> ID;

  int<lower=1> N_pred;
  vector[N_pred] temp_pred;
}

parameters {
  real<lower=min(temp),upper=max(temp)> Topt;

  real<lower=0> dTmin;   //dist. bet. lowest + speed temp. and est. Tmin
  real<lower=0> dTmax; //dist. bet. highest + speed temp. and est. Tmax

  real<lower=0> rmax;

  real<lower=0> beta; //thermal breadth

  // Residual coefficient of variation
  // SD(speed) = cv * expected speed
  real<lower=0> cv;

  real b_weight;
 // vector[N_ID] z_ID;
 // real<lower=0> sd_ID;
}

transformed parameters {
  real Tmin=min(temp) - dTmin;
  real Tmax =max(temp) + dTmax;

  real alpha =(Topt - Tmin)/(Tmax - Tmin);

  //vector[N_ID] ID_effect=sd_ID * z_ID;
}

model {
  Topt ~ normal(0.5 *(min(temp)+max(temp)), 0.5*(max(temp)-min(temp)));

  dTmin ~ lognormal(log(2.5),0.6);

  dTmax ~ lognormal(log(2.5),0.6);

  rmax ~ normal(0,2);

  // assume narrow but not too much, mean beta = 0.25 SD beta   = 0.12
  beta ~ gamma(square(0.25)/square(0.12),0.25/square(0.12));

  cv ~ lognormal(log(0.5),0.5);

  b_weight ~ normal(0, 0.5); //weight eff
 // z_ID ~ std_normal();
 // sd_ID ~ normal(0,0.5);

  for (i in 1:N) {

    real mu_tpc=flex_tpc(temp[i],Tmin,Tmax,rmax,alpha,beta);

    real mu =mu_tpc *exp(b_weight*weight[i]); //add weight eff
   
    real shape=1 / square(cv);

    real rate=shape / mu;

    speed[i] ~ gamma(shape,rate);
  }
}

generated quantities {

  vector[N_pred] speed_pred;

  vector[N] speed_rep;

  for (i in 1:N_pred) {

    speed_pred[i]=flex_tpc(temp_pred[i],Tmin,Tmax,rmax,alpha,beta);
  }

  for (i in 1:N) {

    real mu=flex_tpc(temp[i],Tmin,Tmax,rmax,alpha,beta);
    
   // real mu =mu_population *exp(ID_effect[ID[i]]);

    real shape= 1 / square(cv);

    real rate =shape / mu;


    speed_rep[i] =gamma_rng(shape,rate);
  }
}

