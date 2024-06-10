data {
  // Metadata
  int N;      // no. of participants
  int T;      // no. trials
  int n_options; // no. of options
  
  // Indices
  array[T] int p_ix; // Particpant number for each datapoint
 
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
  real beta_mu_pr; // Inverse temperature 
  real eta_mu_pr;  // Learning rate
  
  // Group-level SDs
  real<lower=0> beta_sigma_pr; // Inverse temperature
  real<lower=0> eta_sigma_pr;  // Learning rate
  
  // Participant-level parameters
  vector[N] beta_pr; // Inverse temperature
  vector[N] eta_pr;  // Learning rate
}

transformed parameters{
  vector[N] beta; // Inverse temperature
  vector[N] eta;  // Learning rate
  
  for (loop_p_ix in 1:N){
    beta[loop_p_ix] = exp(beta_mu_pr + beta_sigma_pr * beta_pr[loop_p_ix]);
    eta[loop_p_ix]  = Phi_approx(eta_mu_pr + eta_sigma_pr * eta_pr[loop_p_ix]);
  }
}

model{
  // Group-level priors for means
  beta_mu_pr ~ normal(0, 1);
  eta_mu_pr  ~ normal(0, 1);
  
  // Group-level priors for SDs
  beta_sigma_pr ~ exponential(0.1);
  eta_sigma_pr  ~ exponential(0.1);
  
  // participant-level priors
  beta_pr ~ normal(0, 1);
  eta_pr  ~ normal(0, 1);

  // containers
  vector[2]  Q;     // Q-values of each option
  vector[T]  Q_diff; // difference in Q values
  vector[T]  alpha;  // parameter for bernoulli logit
  
  // fill utilities with calculated options
  for (trial_ix in 1:T){
   
   // intialise Q-values for a new participant
   if (trial_ix == 1){
     Q = rep_vector(reward/n_options, 2);
   }
   
   // Calculate difference in Q-values
   Q_diff[trial_ix] = Q[2] - Q[1];
   
   // Calculate parmater for bernoulli logit
   alpha[trial_ix] = beta[p_ix[trial_ix]] * Q_diff[trial_ix];
   
   // Update Q values
   Q[actions[trial_ix] + 1] +=  eta[p_ix[trial_ix]] * (outcomes[trial_ix] - Q[actions[trial_ix] + 1]);
   
   
   // Specify probability
   actions[trial_ix] ~ bernoulli_logit(alpha[trial_ix]);
  }
}

generated quantities {
  // containers for transformed means of beta
  real beta_mu = exp(beta_mu_pr);
  real eta_mu  = Phi_approx(eta_mu_pr);
  
  // containers
  vector[2]  Q;     // Q-values of each option
  vector[T]  Q_diff; // difference in Q values
  vector[T]  alpha;  // parameter for bernoulli logit
  
  vector[T] choice_log_lik; // container for choice log likelihoods
  vector[T] choice_pred;    // container for choice predictions
  
  // fill utilities with calculated options
  for (trial_ix in 1:T){
   
   // intialise Q-values for a new participant
   if (trial_ix == 1){
     Q = rep_vector(reward/n_options, 2);
   }
   
   // Calculate difference in Q-values
   Q_diff[trial_ix] = Q[2] - Q[1];
   
   // Calculate parmater for bernoulli logit
   alpha[trial_ix] = beta[p_ix[trial_ix]] * Q_diff[trial_ix];
   
   // Update Q values
   Q[actions[trial_ix] + 1] +=  eta[p_ix[trial_ix]] * (outcomes[trial_ix] - Q[actions[trial_ix] + 1]);
   
  // Choice log likelihood
  // lpmf = log prob mass function
  choice_log_lik[trial_ix] = bernoulli_logit_lpmf(actions[trial_ix] | alpha[trial_ix]);
  
  // Choice predictions
  choice_pred[trial_ix] = bernoulli_logit_rng(alpha[trial_ix]); 
  }
}
