data {
  // Metadata
  int N;      // no. of participants
  int T;      // no. trials
  int n_options; // no. of options
  
  // Indices
  array[T] int p_ix;      // Participant number for each datapoint
  array[T] int condition; // Condition, 0 = control, 1 = experimental
  array[T] int trial_no;  // Trial no
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
   // Group level means
   real beta_mu_pr;  // Inverse temperature
   real eta_mu_pr;   // Learning rate
   real delta_eta_mu_pr; // Offset in learning rate
   
   // Group level SDs
   real<lower=0> beta_sigma_pr;  // Inverse temperature
   real<lower=0> eta_sigma_pr;   // Learning rate
   real<lower=0> delta_eta_sigma_pr;  // Offset in Learning Rate

   // Participant level parameters
   vector[N] beta_pr;  // Inverse temperature
   vector[N] eta_pr;   // Learning rate
   vector[N] delta_eta_pr; // Offset in Learning rate
}

transformed parameters{
   vector[N] beta;        // Inverse temperature
   vector[N] eta;         // Learning rate
   vector[N] delta_eta;    // Offset in learning rate
   vector[N] eta_exp;      // Learning rate post-experimental manipulation
   
   
   for (loop_p_ix in 1:N){
      // Baseline parameters
      beta[loop_p_ix] = Phi_approx(beta_mu_pr + beta_sigma_pr * beta_pr[loop_p_ix]) * 30;
      eta[loop_p_ix]  = Phi_approx(eta_mu_pr + eta_sigma_pr * eta_pr[loop_p_ix]);
      
      // Offset parameter (untransformed)
      delta_eta[loop_p_ix] = delta_eta_mu_pr + delta_eta_sigma_pr * delta_eta_pr[loop_p_ix];

      // Parameters post-experimental manipulation
      eta_exp[loop_p_ix] = Phi_approx(eta_mu_pr + eta_sigma_pr * eta_pr[loop_p_ix] + delta_eta[loop_p_ix]);
   }
}

model{
   
   // Specifiy priors ------------------------------------------//
   // Group level priors
   beta_mu_pr      ~ normal(0,1);
   eta_mu_pr       ~ normal(0,1);
   delta_eta_mu_pr ~ normal(0,1);
   
   // Group level priors for SDs
   beta_sigma_pr       ~ exponential(1);  
   eta_sigma_pr        ~ exponential(1);     
   delta_eta_sigma_pr  ~ exponential(1);    
   
   // participant-level priors
   beta_pr       ~ normal(0, 1);
   eta_pr        ~ normal(0, 1);
   delta_eta_pr  ~ normal(0, 1);

   // Containers ----------------------------------------------//
   vector[2]  Q;     // Q-values of each option
   vector[T]  Q_diff; // difference in Q values
   vector[T]  alpha;  // parameter for bernoulli logit
   
   // Model --------------------------------------------------//
   // fill utilities with calculated options
   for (trial_ix in 1:T){
      // Initialise Q-values if trial id =  1
      if (trial_no[trial_ix] == 1){
         Q = rep_vector(reward/n_options, 2);
      }
      
      
      // Calculate difference in Q-values
      Q_diff[trial_ix] = Q[2] - Q[1];
      
      // Calculate parmater for bernoulli logit
      alpha[trial_ix] = beta[p_ix[trial_ix]] * Q_diff[trial_ix];
      
      // Update Q values
      // Conditional check: 0 = no offset; 1 = offset
      if (condition[trial_ix]==0){
         Q[actions[trial_ix] + 1] +=  eta[p_ix[trial_ix]] * (outcomes[trial_ix] - Q[actions[trial_ix] + 1]);
      } else if(condition[trial_ix]==1){
         Q[actions[trial_ix] + 1] += eta_exp[p_ix[trial_ix]] * (outcomes[trial_ix] - Q[actions[trial_ix] +1]);
      }
      
      // Specify probability
      actions[trial_ix] ~ bernoulli_logit(alpha[trial_ix]);
   }
}

generated quantities {
  
  // Containers for transformed group means ------------- //
  real beta_mu = Phi_approx(beta_mu_pr) * 30;
  real eta_mu  = Phi_approx(eta_mu_pr);
  real delta_eta_mu = delta_eta_mu_pr;
  
  
 // containers
 vector[2]  Q;     // Q-values of each option
 vector[T]  Q_diff; // difference in Q values
 vector[T]  alpha;  // parameter for bernoulli logit
 
 vector[T] choice_log_lik; // container for choice log likelihoods
 vector[T] choice_pred;    // container for choice predictions
 
 // fill utilities with calculated options
   for (trial_ix in 1:T){
      // Initialise Q-values if trial id =  1
      if (trial_no[trial_ix] == 1){
         Q = rep_vector(reward/n_options, 2);
      }
      
      // Calculate difference in Q-values
      Q_diff[trial_ix] = Q[2] - Q[1];
      
      // Calculate parmater for bernoulli logit
      alpha[trial_ix] = beta[p_ix[trial_ix]] * Q_diff[trial_ix];
      
      // Update Q values
      // Conditional check: 0 = no offset; 1 = offset
      if (condition[trial_ix]==0){
         Q[actions[trial_ix] + 1] +=  eta[p_ix[trial_ix]] * (outcomes[trial_ix] - Q[actions[trial_ix] + 1]);
      } else if(condition[trial_ix]==1){
         Q[actions[trial_ix] + 1] += eta_exp[p_ix[trial_ix]] * (outcomes[trial_ix] - Q[actions[trial_ix] +1]);
      }
      
      // Choice log likelihood
      // lpmf = log prob mass function
      choice_log_lik[trial_ix] = bernoulli_logit_lpmf(actions[trial_ix] | alpha[trial_ix]);
      // Choice predictions
      choice_pred[trial_ix] = bernoulli_logit_rng(alpha[trial_ix]); 
   }
}
