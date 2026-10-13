// ============================================================
  // HURDLE MODEL FOR THERMAL PERFORMANCE CURVES
//
  // Submodel 1:
  //   P(movement | temperature)
//
  // Submodel 2:
  //   E(speed | movement, temperature)
//
  // Overall locomotor performance:
  //   P(movement) * E(speed | movement)
// ============================================================
  
  
  functions {
    
    // ==========================================================
      // PROBABILITY OF MOVEMENT
    //
      // Movement probability:
      //   - increases at low temperatures
    //   - remains high at intermediate temperatures
    //   - decreases at high temperatures
    //
      // pmax:
      //   upper ceiling for probability of movement
    //
      // Tcold:
      //   midpoint of the cold-side transition
    //
      // Thot:
      //   midpoint of the hot-side transition
    //
      // kcold:
      //   steepness of the increase around Tcold
    //
      // khot:
      //   steepness of the decline around Thot
    //
      // Larger k = sharper transition
      // Smaller k = more gradual transition
      //
        // Units of k are approximately 1 / degree C
      // ==========================================================
        
        real p_move(
          real temp,
          real pmax,
          real Tcold,
          real Thot,
          real kcold,
          real khot
        ) {
          
          // Cold response increases from 0 -> 1
          // as temperature increases
          real cold_response =
            inv_logit(
              kcold * (temp - Tcold)
            );
          
          
          // Hot response decreases from 1 -> 0
          // as temperature increases
          real hot_response =
            inv_logit(
              khot * (Thot - temp)
            );
          
          
          return pmax *
            cold_response *
            hot_response;
        }
      
      
      
      // ==========================================================
        // FLEXTPC FOR SPEED
      //
        // Tmin:
        //   lower thermal limit
      //
        // Tmax:
        //   upper thermal limit
      //
        // rmax:
        //   maximum expected speed
      //
        // alpha:
        //   controls where Topt occurs between Tmin and Tmax
      //
        //   Topt =
          //     alpha*Tmax +
          //     (1-alpha)*Tmin
        //
          // beta:
          //   controls breadth of thermal performance curve
        //
          //   small beta = narrow / peaked
          //   large beta = broad
          // ==========================================================
            
            real flex_tpc(
              real temp,
              real Tmin,
              real Tmax,
              real rmax,
              real alpha,
              real beta
            ) {
              
              // Outside the thermal limits,
              // expected speed = 0
              if (temp <= Tmin || temp >= Tmax)
                return 0;
              
              
              // Shape parameter
              real s =
                alpha *
                (1 - alpha) /
                square(beta);
              
              
              return rmax * exp(
                
                s * (
                  
                  alpha *
                    log(
                      (temp - Tmin) /
                        alpha
                    )
                  
                  +
                    
                    (1 - alpha) *
                    log(
                      (Tmax - temp) /
                        (1 - alpha)
                    )
                  
                  -
                    
                    log(
                      Tmax - Tmin
                    )
                )
              );
            }
          
  }



data {
  
  // ==========================================================
    // NUMBER OF INDIVIDUALS
  // ==========================================================
    
    int<lower=1> N_ID;
    
    
    // ==========================================================
      // MOVEMENT DATA
    //
      // Includes both movers and non-movers
    // ==========================================================
      
      int<lower=1> N;
      
      vector[N] temp;
      
      array[N]
      int<lower=0, upper=1> moved;
      
      array[N]
      int<lower=1, upper=N_ID> ID;
      
      
      // ==========================================================
        // SPEED DATA
      //
        // Includes ONLY observations where:
        //   moved == 1
      //   speed > 0
      // ==========================================================
        
        int<lower=1> N_speed;
      
      vector[N_speed] temp_speed;
      
      vector<lower=0>[N_speed] speed;
      
      array[N_speed]
      int<lower=1, upper=N_ID> ID_speed;
      
      
      // ==========================================================
        // TEMPERATURE GRID FOR PREDICTIONS
      // ==========================================================
        
        int<lower=1> N_pred;
      
      vector[N_pred] temp_pred;
}



parameters {
  
  // ==========================================================
    // MOVEMENT PARAMETERS
  // ==========================================================
    
    // Upper ceiling for probability of movement
  real<lower=0, upper=1> pmax;
  
  
  // Ordered so that:
    //
    // Tmove[1] = cold transition
  // Tmove[2] = hot transition
  //
    // and:
    //
    // Tmove[1] < Tmove[2]
  ordered[2] Tmove;
  
  
  // Steepness of cold-side increase
  real<lower=0> kcold;
  
  
  // Steepness of hot-side decrease
  real<lower=0> khot;
  
  
  // Individual-level variation
  // in tendency to move
  vector[N_ID] z_move;
  
  real<lower=0> sd_move;
  
  
  
  // ==========================================================
    // SPEED TPC PARAMETERS
  // ==========================================================
    
    // Lower thermal limit.
  //
    // Constrained between:
    //
    // lowest temperature tested
  //
    // and
  //
    // lowest temperature where positive speed was observed
  real<upper=min(temp_speed)> Tmin;
  
  
  // Upper thermal limit.
  //
    // Constrained between:
    //
    // highest temperature with positive speed
  //
    // and
  //
    // highest temperature tested
  real<lower=max(temp_speed)> Tmax;
  
  
  // Maximum expected speed
  real<lower=0> rmax;
  
  
  // Position of Topt between Tmin and Tmax
  real<lower=0, upper=1> alpha;
  
  
  // Thermal breadth
  real<lower=0> beta;
  
  
  
  // ==========================================================
    // RESIDUAL VARIATION IN SPEED
  //
    // cv = coefficient of variation
  //
    // SD(speed) = cv * expected speed
  //
    // This replaces a constant sigma.
  // It works better when expected speeds can approach zero.
  // ==========================================================
    
    real<lower=0> cv;
  
  
  
  // ==========================================================
    // INDIVIDUAL DIFFERENCES IN SPEED
  // ==========================================================
    
    vector[N_ID] z_speed;
  
  real<lower=0> sd_speed;
}



model {
  
  // ==========================================================
    // MOVEMENT PRIORS
  // ==========================================================
    
    // Mild preference for relatively high
  // maximum movement probability
  pmax ~ beta(2, 1);
  
  
  // Cold-side transition
  Tmove[1] ~ normal(
    min(temp),
    10
  );
  
  
  // Hot-side transition
  Tmove[2] ~ normal(
    max(temp),
    10
  );
  
  
  // Steepness of temperature responses
  kcold ~ lognormal(
    log(0.3),
    0.8
  );
  
  khot ~ lognormal(
    log(0.3),
    0.8
  );
  
  
  
  // ==========================================================
    // SPEED TPC PRIORS
  // ==========================================================
    
    // Thermal limits
  Tmin ~ normal(
    min(temp_speed) - 2.5,
    5
  );
  
  Tmax ~ normal(
    max(temp_speed) + 2.5,
    5
  );
  
  
  // Maximum locomotor speed
  rmax ~ normal(
    0,
    10
  );
  
  
  // Mild preference for an interior Topt
  alpha ~ beta(
    2,
    2
  );
  
  
  // Mild preference for a clearly peaked
  // but not extremely narrow TPC
  //
    // prior mean = 0.25
  // prior SD   = 0.12
  beta ~ gamma(
    square(0.25) / square(0.12),
    0.25 / square(0.12)
  );
  
  
  
  // ==========================================================
    // RESIDUAL VARIATION
  //
    // Median CV = 0.5
  //
    // Rough interpretation:
    //
    // cv = 0.5 means residual SD is about
  // 50% of expected speed
  // ==========================================================
    
    cv ~ lognormal(
      log(0.5),
      0.5
    );
  
  
  
  // ==========================================================
    // INDIVIDUAL RANDOM EFFECTS
  // ==========================================================
    
    // Standardized random effects
  z_move ~ normal(
    0,
    1
  );
  
  z_speed ~ normal(
    0,
    1
  );
  
  
  // Among-individual variation
  sd_move ~ normal(
    0,
    1
  );
  
  sd_speed ~ normal(
    0,
    0.5
  );
  
  
  
  // ==========================================================
    // 1. MOVEMENT MODEL
  // ==========================================================
    
    for (i in 1:N) {
      
      // Population-level movement probability
      real p = p_move(
        temp[i],
        pmax,
        Tmove[1],
        Tmove[2],
        kcold,
        khot
      );
      
      
      // Add individual-level effect
      // on the logit scale
      real p_ind = inv_logit(
        
        logit(p)
        
        +
          
          sd_move *
          z_move[ID[i]]
      );
      
      
      moved[i] ~ bernoulli(
        p_ind
      );
    }
  
  
  
  // ==========================================================
    // 2. SPEED MODEL
  //
    // Only positive speed observations
  // from individuals that moved
  // ==========================================================
    
    for (i in 1:N_speed) {
      
      // Expected speed from flexTPC
      real mu = flex_tpc(
        temp_speed[i],
        Tmin,
        Tmax,
        rmax,
        alpha,
        beta
      );
      
      
      // Allow individuals to differ
      // in their general speed
      mu *= exp(
        
        sd_speed *
          z_speed[ID_speed[i]]
        
      );
      
      
      // --------------------------------------------------------
        // Gamma likelihood with constant coefficient of variation
      //
        // If:
        //
        // SD = cv * mu
      //
        // then:
        //
        // shape = 1 / cv^2
      //
        // rate = shape / mu
      // --------------------------------------------------------
        
        real shape =
        1 / square(cv);
      
      
      real rate =
        shape / mu;
      
      
      speed[i] ~ gamma(
        shape,
        rate
      );
    }
  
}



generated quantities {
  
  // ==========================================================
    // POSTERIOR PREDICTION CURVES
  // ==========================================================
    
    vector[N_pred] movement_pred;
  
  vector[N_pred] speed_pred;
  
  vector[N_pred] overall_pred;
  
  
  // ==========================================================
    // OPTIMUM OF CONDITIONAL SPEED TPC
  //
    // IMPORTANT:
    // This is NOT necessarily the optimum of overall
  // locomotor performance.
  // ==========================================================
    
    real Topt_speed =
      alpha * Tmax +
      (1 - alpha) * Tmin;
    
    
    
    // ==========================================================
      // PREDICTION CURVES
    // ==========================================================
      
      for (i in 1:N_pred) {
        
        // Probability of moving
        movement_pred[i] =
          p_move(
            temp_pred[i],
            pmax,
            Tmove[1],
            Tmove[2],
            kcold,
            khot
          );
        
        
        // Expected speed conditional on moving
        speed_pred[i] =
          flex_tpc(
            temp_pred[i],
            Tmin,
            Tmax,
            rmax,
            alpha,
            beta
          );
        
        
        // Overall expected locomotor performance
        //
          // E(speed)
        //
          // =
          //
          // P(move) *
          // E(speed | move)
        overall_pred[i] =
          movement_pred[i] *
          speed_pred[i];
      }
    
}

