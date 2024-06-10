data {
  // Metadata
  int N;      // no. of participants
  int T;      // no. trials
  int n_options; // no. of options
  
  // Indices
  array[T] int p_ix; // Particpant number for each datapoint
  array[T] int trial_no;  // trial number
 
  // Data
  array[T] int actions;          // Dependent variable: action taken (0 = option 1; 1 = option 2)
  array[T] int outcomes;         // Outcome (0 = no reward; 1 = reward)
  array[T] int time_since_reversal; // time since last reversal at trial t
}

transformed data{
  // Task information
  real pr_win_given_correct   = .70;
  real pr_win_given_incorrect = 1 - pr_win_given_correct;
  real pr_switch              = .10;
  
  real reward                 = 1;
}

parameters{
  // Group-level means
  real beta_G_mu_pr;  // Inverse temperature for GO
  real beta_NG_mu_pr; // Inverse temperature for NOGO
  real eta_G_mu_pr;   // Learning rate for GO
  real eta_NG_mu_pr;  // Learning rate for NOGO
  
  // Group-level SDs
  real<lower=0> beta_G_sigma_pr;  // Inverse temperature for GO
  real<lower=0> beta_NG_sigma_pr; // Inverse temperature for NOGO
  real<lower=0> eta_G_sigma_pr;   // Learning rate for GO 
  real<lower=0> eta_NG_sigma_pr;  // Learning rate for NOGO
  
  // Participant-level parameters
  vector[N] beta_G_pr;  // Inverse temperature for GO
  vector[N] beta_NG_pr; // Inverse temperature for NOGO
  vector[N] eta_G_pr;   // Learning rate for GO
  vector[N] eta_NG_pr;  // Learning rate for NOGO
}

transformed parameters{
  vector[N] beta_G;  // Inverse temperature for GO
  vector[N] beta_NG; // Inverse temperature for NOGO
  vector[N] eta_G;   // Learning rate for GO
  vector[N] eta_NG;  // Learning rate for NOGO
  
  for (loop_p_ix in 1:N){
    beta_G[loop_p_ix]  = Phi_approx(beta_G_mu_pr + beta_G_sigma_pr   * beta_G_pr[loop_p_ix]) * 2;
    beta_NG[loop_p_ix] = Phi_approx(beta_NG_mu_pr + beta_NG_sigma_pr * beta_NG_pr[loop_p_ix]) * 2;
    eta_G[loop_p_ix]   = Phi_approx(eta_G_mu_pr + eta_G_sigma_pr     * eta_G_pr[loop_p_ix]);
    eta_NG[loop_p_ix]  = Phi_approx(eta_NG_mu_pr + eta_NG_sigma_pr   * eta_NG_pr[loop_p_ix]);
  }
}

model{
  // Group-level priors for means
  beta_G_mu_pr  ~ normal(0, 1);
  beta_NG_mu_pr ~ normal(0, 1);
  eta_G_mu_pr   ~ normal(0, 1);
  eta_NG_mu_pr  ~ normal(0, 1);
  
  // Group-level priors for SDs
  beta_G_sigma_pr  ~ exponential(1);
  beta_NG_sigma_pr ~ exponential(1);
  eta_G_sigma_pr   ~ exponential(1);
  eta_NG_sigma_pr  ~ exponential(1);
  
  // participant-level priors
  beta_G_pr  ~ normal(0, 1);
  beta_NG_pr ~ normal(0, 1);
  eta_G_pr   ~ normal(0, 1);
  eta_NG_pr  ~ normal(0, 1);

  // containers
  vector[2]  G;        // G weights of each option
  vector[2]  NG;       // NG weights of each option
  vector[2]  act;      // actor weights for choice A and B
  vector[T]  act_diff; // difference in Q values
  vector[T]  alpha;  // parameter for bernoulli logit
  
  // fill utilities with calculated options
  for (trial_ix in 1:T){
   
   // intialise Q-values for a new participant
   if (trial_no[trial_ix] == 1){
     G  = rep_vector(reward/n_options, 2);
     NG = rep_vector(reward/n_options, 2);
   }
   
   // Calculate Actor weights for each action
   act[1] = beta_G[p_ix[trial_ix]] * G[1] - beta_NG[p_ix[trial_ix]] * NG[1];
   act[2] = beta_G[p_ix[trial_ix]] * G[2] - beta_NG[p_ix[trial_ix]] * NG[2];
   
   // Calculate difference in Actor weights
   act_diff[trial_ix] = act[2] - act[1];
   
   // Calculate parameter for bernoulli logit
   alpha[trial_ix] = act_diff[trial_ix];
   
   // Update G and NG weights for next trial
   G[actions[trial_ix] + 1]  += (eta_G[p_ix[trial_ix]] * G[actions[trial_ix] + 1]) * (outcomes[trial_ix] - G[actions[trial_ix] + 1]);
   NG[actions[trial_ix] + 1] += (eta_NG[p_ix[trial_ix]] * NG[actions[trial_ix] + 1]) * (NG[actions[trial_ix] + 1] - outcomes[trial_ix]);
   
   // Specify probability
   actions[trial_ix] ~ bernoulli_logit(alpha[trial_ix]);
  }
}

generated quantities {
  // containers for transformed means of beta
  real beta_G_mu = Phi_approx(beta_G_mu_pr) * 2;
  real beta_NG_mu = Phi_approx(beta_NG_mu_pr) * 2;
  real eta_G_mu  = Phi_approx(eta_G_mu_pr);
  real eta_NG_mu  = Phi_approx(eta_NG_mu_pr);
  
  // containers
  vector[2]  G;        // G weights of each option
  vector[2]  NG;       // NG weights of each option
  vector[2]  act;      // actor weights for choice A and B
  vector[T]  act_diff; // difference in Q values
  vector[T]  alpha;  // parameter for bernoulli logit
  
  vector[T] choice_log_lik; // container for choice log likelihoods
  vector[T] choice_pred;    // container for choice predictions
  
  // fill utilities with calculated options
  for (trial_ix in 1:T){
   
    // intialise Q-values for a new participant
    if (trial_no[trial_ix] == 1){
     G  = rep_vector(reward/n_options, 2);
     NG = rep_vector(reward/n_options, 2);
    }
    
    // Calculate Actor weights for each action
    act[1] = beta_G[p_ix[trial_ix]] * G[1] - beta_NG[p_ix[trial_ix]] * NG[1];
    act[2] = beta_G[p_ix[trial_ix]] * G[2] - beta_NG[p_ix[trial_ix]] * NG[2];
    
    // Calculate difference in Actor weights
    act_diff[trial_ix] = act[2] - act[1];
    
    // Calculate parameter for bernoulli logit
    alpha[trial_ix] = act_diff[trial_ix];
    
    // Update G and NG weights for next trial
    G[actions[trial_ix] + 1]  += (eta_G[p_ix[trial_ix]] * G[actions[trial_ix] + 1]) * (outcomes[trial_ix] - G[actions[trial_ix] + 1]);
    NG[actions[trial_ix] + 1] += (eta_NG[p_ix[trial_ix]] * NG[actions[trial_ix] + 1]) * (NG[actions[trial_ix] + 1] - outcomes[trial_ix]);
    
    // Choice log likelihood
    // lpmf = log prob mass function
    choice_log_lik[trial_ix] = bernoulli_logit_lpmf(actions[trial_ix] | alpha[trial_ix]);
    
    // Choice predictions
    choice_pred[trial_ix] = bernoulli_logit_rng(alpha[trial_ix]); 
  }
}
