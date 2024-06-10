## Bayesian parameter-free model
model_bayes_pf <- list(
  model_name      = "Bayesian Parameter Free",
  stan_file_noext = "bayes_parfree",
  stan_file       = "bayes_parfree.stan",
  group_pars      = c(
  ),
  indiv_pars      = c(
    "beta"
  ),
  init            = NULL
)

## Bayesian updating model with learning rate parameter (eta) 
model_bayes_learnrate <- list(
  model_name      = "Bayesian Learning Rate",
  stan_file_noext = "bayes_learnrate",
  stan_file       = "bayes_learnrate.stan",
  group_pars      = c(
  ),
  indiv_pars      = c(
    "eta",
    "beta"
  ),
  init            = NULL
)



## Bayesian updating model with precision weighting parameters (w_prior, w_evidence) 
model_bayes_precis <- list(
  model_name      = "Bayesian Precision Weighting",
  stan_file_noext = "bayes_precis",
  stan_file       = "bayes_precis.stan",
  group_pars      = c(
  ),
  indiv_pars      = c(
    "w_prior",
    "w_evidence",
    "beta"
  ),
  init            = NULL
)

## Q Learning model with learning rate (eta)

model_q_learning <- list(
  model_name      = "Q Learning",
  stan_file_noext = "qlearning",
  stan_file       = "qlearning.stan",
  group_pars      = c(),
  indiv_pars      = c(
    "eta",
    "beta"
  ),
  init            =  NULL
)

# Models with Partial Pooling ---------------------------------------------

## Q Learning model with partial pooling
model_q_learning_pp <- list(
  model_name      = "Q Learning with Partial Pooling",
  stan_file_noext = "qlearning_pp",
  stan_file       = "qlearning_pp.stan",
  group_pars      = c(
    "beta_mu",
    "beta_mu_pr",
    "beta_sigma_pr",
    
    "eta_mu",
    "eta_mu_pr",
    "eta_sigma_pr"
  ),
  indiv_pars      = c(
    "eta",
    "beta"
  ),
  init            =  NULL
)

## Bayesian parameter-free model with partial pooling
model_bayes_pf_pp <- list(
  model_name      = "Bayesian Parameter Free with Partial Pooling",
  stan_file_noext = "bayes_parfree_pp",
  stan_file       = "bayes_parfree_pp.stan",
  group_pars      = c(
    "beta_mu",
    "beta_mu_pr",
    "beta_sigma_pr"
  ),
  indiv_pars      = c(
    "beta"
  ),
  init            = NULL
)


## Bayesian updating model with learning rate parameter (eta) and Partial Pooling
model_bayes_learnrate_pp <- list(
  model_name      = "Bayesian Learning Rate with Partial Pooling",
  stan_file_noext = "bayes_learnrate_pp",
  stan_file       = "bayes_learnrate_pp.stan",
  group_pars      = c(
    "beta_mu",
    "beta_mu_pr",
    "beta_sigma_pr",
    "eta_mu", 
    "eta_mu_pr",
    "eta_sigma_pr"
  ),
  indiv_pars      = c(
    "beta",
    "eta"
  ),
  init            = NULL
)

## Bayesian updating model with precision weighting parameters (w_prior, w_evidence) and partial pooling
model_bayes_precis_pp <- list(
  model_name      = "Bayesian Precision Weighting with Partial Pooling",
  stan_file_noext = "bayes_precis_pp",
  stan_file       = "bayes_precis_pp.stan",
  group_pars      = c(
    "beta_mu",
    "w_prior_mu",
    "beta_mu_pr",
    "w_prior_mu_pr",
    "beta_sigma_pr",
    "w_prior_sigma_pr"
  ),
  indiv_pars      = c(
    "beta",
    "w_prior",
    "w_evidence"
  ),
  init            = NULL
)


#  Models with Partial Pooling & Offset parameter -------------------------------------------


## Bayesian updating model with learning rate parameter (eta) and Partial Pooling
## with estimation of an Offset parameter
model_bayes_learnrate_pp_offset <- list(
  model_name      = "Bayesian Learning Rate with Partial Pooling, with offset parameter",
  stan_file_noext = "bayes_learnrate_pp_offset",
  stan_file       = "bayes_learnrate_pp_offset.stan",
  group_pars      = c(
    "beta_mu",
    "beta_mu_pr",
    "beta_sigma_pr",
    
    "eta_mu", 
    "eta_mu_pr",
    "eta_sigma_pr",
    
    "delta_eta_mu",
    "delta_eta_mu_pr",
    "delta_eta_sigma_pr"
  ),
  indiv_pars      = c(
    "beta",
    "eta",
    "eta_exp",
    "delta_eta"
  ),
  init            = NULL
)


## Bayesian updating model with precision weighting parameters (w_prior, w_evidence) and partial pooling
## with estimation of an Offset parameter
model_bayes_precis_pp_offset <- list(
  model_name      = "Bayesian Precision Weighting with Partial Pooling, with offset parameter",
  stan_file_noext = "bayes_precis_pp_offset",
  stan_file       = "bayes_precis_pp_offset.stan",
  group_pars      = c(
    "beta_mu",
    "beta_mu_pr",
    "beta_sigma_pr",
    
    "w_prior_mu",
    "w_prior_mu_pr",
    "w_prior_sigma_pr",

    "delta_w_prior_mu",
    "delta_w_prior_mu_pr",
    "delta_w_prior_sigma_pr"
  ),
  indiv_pars      = c(
    "beta",
    "w_prior",
    "w_evidence",
    "w_prior_exp",
    "w_evidence_exp",
    "delta_w_prior"
  ),
  init            = NULL
)



## Q Learning model with learning rate (eta) and partial pooling
## with estimation of an Offset parameter
model_q_learning_pp_offset <- list(
  model_name      = "Q Learning with Partial Pooling, with offset parameter",
  stan_file_noext = "qlearning_pp_offset",
  stan_file       = "qlearning_pp_offset.stan",
  group_pars      = c(
    "beta_mu",
    "beta_mu_pr",
    "beta_sigma_pr",
    
    "eta_mu", 
    "eta_mu_pr",
    "eta_sigma_pr",
    
    "delta_eta_mu",
    "delta_eta_mu_pr",
    "delta_eta_sigma_pr"
  ),
  indiv_pars      = c(
    "beta",
    "eta",
    "eta_exp",
    "delta_eta"
  ),
  init            =  NULL
)

## Opponent actor learning (OpAL) model from Collins & Frank 2014

model_opal_pp <- list(
  model_name = "Opponent Actor Learning (OpAL) with Partial Pooling",
  stan_file_noext = "opal_pp",
  stan_file       = "opal_pp.stan",
  group_pars      = c(
    "beta_G_mu",
    "beta_G_mu_pr",
    "beta_G_sigma_pr",
    
    "beta_NG_mu",
    "beta_NG_mu_pr",
    "beta_NG_sigma_pr",
    
    "eta_G_mu",
    "eta_G_mu_pr",
    "eta_G_sigma_pr",
    
    "eta_NG_mu",
    "eta_NG_mu_pr",
    "eta_NG_sigma_pr"
  ),
  indiv_pars      = c(
    "beta_G",
    "beta_NG",
    "eta_G",
    "eta_NG"
  ),
  init = NULL
)

