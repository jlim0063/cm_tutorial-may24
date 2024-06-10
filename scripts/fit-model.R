# Load packages
require(cmdstanr)
require(dplyr)
require(data.table)
require(tictoc)


# Set cmdstan path according to machine used
system_cmdstan_path <- c("C:/Users/aarka/Documents/.cmdstan/cmdstan-2.32.0", 
                         "C:/Users/gymno/Documents/.cmdstan/cmdstan-2.32.0")
for (path in system_cmdstan_path){
  if (file.exists(path)){
    set_cmdstan_path(path)
  }
}


# Load data
sim_data_QL   <- read.csv(here::here("data/simulated_data_2.csv")) %>% as.data.table()
sim_data_opAL <- read.csv(here::here("data/simulated_data_opAL.csv")) %>% as.data.table()

# Pick which dataset to run
sim_data <- sim_data_QL

# Set run model flag
run_model_flag <- F

# Load previously saved data?
load_rdata_flag <- T
if (load_rdata_flag == T){
  load(here::here("output/m1_model_objects.Rdata"))
  load(here::here("output/m2_model_objects.Rdata"))
}

## For the purpose of our tutorial, we will run both the QLearning model (Rescorla-Wagner) and the 
## Opponent Actor Learning (OpAL) model on `sim_data_QL`, just to demonstrate model comparisons.
## For parameter recovery, you should be comparing each model's estimates to the corresponding 
## simulated dataset, because it doesn't make sense to compare estimated values to a wholly different
## set of parameters used to simulate the data.

# Prep for stan modelling -----------------------------------------

## Get no. of unique participants 
n_participants <- sim_data$subject %>% unique() %>% length()
n_trials       <- sim_data$trial_no %>% unique() %>% length()
n_conditions   <- sim_data$condition %>% unique() %>% length()

## Re-index participants for stan modelling
for (ID in unique(sim_data$subject)){
  index <- which(unique(sim_data$subject)==ID)
  sim_data[subject == ID, stan_index:=seq(1, n_participants)[index]]
}

## Re-index condition for stan modelling
sim_data[, condition_index:=ifelse(condition == "pre", 0, 1)]

# Organise data for Stan
stan_data <- list(
  ## Metadata
  N         = n_participants,            # Number of participants 
  T         = n_participants * n_trials * n_conditions, # Number of trials
  n_options = 2,
  
  ## Indices
  p_ix      = sim_data$stan_index,      # Index of participant id
  trial_no  = sim_data[, trial_no],     # Index of trial numbers   
  condition = sim_data$condition_index, # Index of condition
  
  ## Data
  actions           = sim_data[, ptresp] ,                # Participant response
  outcomes          = sim_data[, outcome],                # Outcome of action (0 = nonreward, 1  = reward) 
  time_since_reversal = sim_data[, trials_since_reversal] # No. of trials since last reversal
)


# Load model library ------------------------------------------------------

source(here::here("scripts", "model_library.R"))
model_stan_dir <- here::here('scripts', 'stan-files')


# M1: Q Learning with Partial Pooling -------------------------------------

model_to_fit   <- model_q_learning_pp_offset

## Remove stan model .exe file if already exists
if(file.exists(here::here('scripts', 'stan-files', paste(model_to_fit$stan_file_noext, '.exe', sep = ""))) == T){
  file.remove(here::here('scripts', 'stan-files', paste(model_to_fit$stan_file_noext, '.exe', sep = "")))
}

## Pre-compile model
compiled_model <- cmdstan_model(
  stan_file       = here::here('scripts', 'stan-files', model_to_fit$stan_file),
  force_recompile = T
)


## Create containers for participant-level estimates
m1_est_beta      <- rep(NA, times = n_participants) ## Inverse temperature
m1_est_eta       <- rep(NA, times = n_participants) ## learning rate 
m1_est_delta_eta <- rep(NA, times = n_participants) ## Offset in learning rate in 'post' condition

m1_beta_within_50ci       <- rep(NA, times = n_participants)  ## Inverse temperature
m1_beta_within_90ci       <- rep(NA, times = n_participants)  ## Inverse temperature
m1_eta_within_50ci        <- rep(NA, times = n_participants)  ## Learning rate
m1_eta_within_90ci        <- rep(NA, times = n_participants)  ## Learning rate
m1_delta_eta_within_50ci        <- rep(NA, times = n_participants)  ## Learning rate
m1_delta_eta_within_90ci        <- rep(NA, times = n_participants)  ## Learning rate


## Sampling
if(run_model_flag){
  tic()
  m1_fit <- compiled_model$sample(
    data            = stan_data,
    chains          = 4,
    parallel_chains = 4,
    refresh         = 100,
    iter_warmup     = 500,
    iter_sampling   = 1000,
    save_warmup     = FALSE
  )
  toc()
  
  ## Play audio cue
  beepr::beep("fanfare")
}

## Save data
if (run_model_flag){
  ## Print and/or save samples
  m1_fit$save_output_files(
    dir      = here::here("output"),
    basename = model_to_fit$model_name
  )
}
  

# Extract variables ---------------- 

output_files <-  m1_fit$output_files()

## extract log-likelihood matrix
m1_log_likelihood <- read_cmdstan_csv(
  files               = output_files,
  variables           = c("choice_log_lik"),
  sampler_diagnostics = NULL,
  format              = getOption("cmdstanr_draws_format", "draws_df")
)

## extract predicted choices
m1_choice_pred <- read_cmdstan_csv(
  files               = output_files,
  variables           = c("choice_pred"),
  sampler_diagnostics = NULL,
  format              = getOption("cmdstanr_draws_format", "draws_df")
)

## get WAIC for model
m1_ll_samples <- as.matrix(
  m1_log_likelihood$post_warmup_draws[, 1:m1_log_likelihood$metadata$stan_variable_sizes$choice_log_lik,]
)
m1_model_WAIC <- loo::waic(m1_ll_samples)


#####################################################
## Extract parameter samples (Group-level)
#####################################################

m1_group_par_samples_all <- read_cmdstan_csv(
  files = output_files,
  variables = model_to_fit$group_pars,
  sampler_diagnostics = NULL,
  format = getOption("cmdstanr_draws_format", "draws_df")
)

## Get median group parameter estimates
m1_group_par_samples <- as.matrix(m1_group_par_samples_all$post_warmup_draws[,1:length(model_to_fit$group_pars)])
m1_group_par_est     <- apply(m1_group_par_samples, MARGIN=2, FUN = median)
m1_group_par_CI      <- apply(m1_group_par_samples, MARGIN = 2, FUN = quantile , probs = c(.025, .5, .975))

## Extract model choices
m1_pred_right_prop <- colMeans(m1_choice_pred$post_warmup_draws)[1:(40*150*2)]

#####################################################
## Extract parameter samples (Individual-level)
#####################################################

m1_indiv_par_samples_all <- read_cmdstan_csv(
  files= output_files,
  variables = model_to_fit$indiv_pars,
  sampler_diagnostics = NULL,
  format = getOption("cmdstanr_draws_format", "draws_df")
)

m1_indiv_par_samples <- vector(mode="list",
                               length=length(model_to_fit$indiv_pars))

m1_indiv_par_est <- matrix(NA, nrow = m1_indiv_par_samples_all$metadata$stan_variable_sizes[[model_to_fit$indiv_pars[1]]],
                           ncol = length(model_to_fit$indiv_pars))
colnames(m1_indiv_par_est) <- model_to_fit$indiv_pars

## Plot distribution for samples of individual participant parameters
for (i in 1:length(m1_indiv_par_samples)){
  m1_indiv_par_samples[[i]] <-
    as.matrix(m1_indiv_par_samples_all$post_warmup_draws[seq(
      from       = 1 + (i-1) * dim(m1_indiv_par_est)[1],
      to         = i * dim(m1_indiv_par_est)[1],
      length.out = dim(m1_indiv_par_est)[1])
    ])
  m1_indiv_par_est[,i] <- apply(m1_indiv_par_samples[[i]], MARGIN=2, FUN=median)
  
  hist(m1_indiv_par_est[,i], main=model_to_fit$indiv_pars[i], 30)
}

## Code to get beta sample distribution for e.g., participant no. 11
pt_id <- "18"
pt_ix <- match(pt_id, sim_data_QL$subject %>% unique())
param <- match("beta", model_q_learning_pp_offset$indiv_pars)  
hist(m1_indiv_par_samples[[param]][, pt_ix], 100, main= paste("Beta  Samples for", pt_id))

## Record median parameter values for each participant
for (pt_ix in 1:n_participants){
  m1_est_beta[pt_ix]       <- m1_indiv_par_samples[[1]][,pt_ix] %>% median()
  m1_est_eta[pt_ix]      <- m1_indiv_par_samples[[2]][,pt_ix] %>% median()
  m1_est_delta_eta[pt_ix] <- m1_indiv_par_samples[[4]][,pt_ix] %>% median()
  
  m1_beta_within_50ci[pt_ix] <- m1_est_beta[pt_ix] > quantile(m1_indiv_par_samples[[1]], 0.25) & m1_est_beta[pt_ix] < quantile(m1_indiv_par_samples[[1]], 0.75)
  m1_beta_within_90ci[pt_ix] <- m1_est_beta[pt_ix] > quantile(m1_indiv_par_samples[[1]], 0.05) & m1_est_beta[pt_ix] < quantile(m1_indiv_par_samples[[1]], 0.95)
  
  m1_eta_within_50ci[pt_ix] <- m1_est_eta[pt_ix] > quantile(m1_indiv_par_samples[[2]], 0.25) & m1_est_eta[pt_ix] < quantile(m1_indiv_par_samples[[2]], 0.75)
  m1_eta_within_90ci[pt_ix] <- m1_est_eta[pt_ix] > quantile(m1_indiv_par_samples[[2]], 0.05) & m1_est_eta[pt_ix] < quantile(m1_indiv_par_samples[[2]], 0.95)
  
  m1_delta_eta_within_50ci[pt_ix] <- m1_est_eta[pt_ix] > quantile(m1_indiv_par_samples[[4]], 0.25) & m1_est_eta[pt_ix] < quantile(m1_indiv_par_samples[[4]], 0.75)
  m1_delta_eta_within_90ci[pt_ix] <- m1_est_eta[pt_ix] > quantile(m1_indiv_par_samples[[4]], 0.05) & m1_est_eta[pt_ix] < quantile(m1_indiv_par_samples[[4]], 0.95)
}

## check calibration
mean(m1_eta_within_50ci); mean(m1_eta_within_90ci)
mean(m1_beta_within_50ci); mean(m1_beta_within_90ci)
mean(m1_delta_eta_within_50ci); mean(m1_delta_eta_within_90ci)

# m1: parameter recovery check (ONLY RUN IF your stan data is from `sim_data_QL`) -----------------------

## Correlations with true parameter values
cor.test(unique(sim_data_QL$true_beta), m1_est_beta)
cor.test(unique(sim_data_QL$true_eta_pre), m1_est_eta)
cor.test(unique(sim_data_QL$true_delta_eta), m1_est_delta_eta)

# m1: save model objects --------------------------------------------------

save(list = ls(pattern = "m1_"), file = here::here("output", "model-Rdata", "m1_model_objects.Rdata"))

##########################################################################################################
## ======================================================================================================
# Model 2 (m2): Bayesian Precision Weighting Model with Partial Pooling --------

model_to_fit   <- model_bayes_precis_pp_offset

## Remove stan model .exe file if already exists
if(file.exists(here::here('scripts', 'stan-files', paste(model_to_fit$stan_file_noext, '.exe', sep = ""))) == T){
  file.remove(here::here('scripts', 'stan-files', paste(model_to_fit$stan_file_noext, '.exe', sep = "")))
}


## Pre-compile model
compiled_model <- cmdstan_model(
  stan_file       = here::here('scripts', 'stan-files', model_to_fit$stan_file),
  force_recompile = T
)

## Create containers for participant-level estimates
m2_est_beta          <- rep(NA, times = n_participants)    ## Inverse temperature
m2_est_w_prior       <- rep(NA, times = n_participants)    ## Weight on prior
m2_est_w_evidence    <- rep(NA, times = n_participants)    ## Weight on evidence
m2_est_w_prior_sr    <- rep(NA, times = n_participants)    ## Weight on prior after SR
m2_est_w_evidence_sr <- rep(NA, times = n_participants)    ## Weight on evidence after SR
m2_est_delta_w_prior <- rep(NA, times = n_participants)    ## Offset parameter: i.e., deviation in w_prior during sleep loss

m2_beta_within_50ci          <- rep(NA, times = n_participants)
m2_beta_within_90ci          <- rep(NA, times = n_participants)
m2_w_prior_within_50ci       <- rep(NA, times = n_participants)
m2_w_prior_within_90ci       <- rep(NA, times = n_participants)
m2_w_evidence_within_50ci    <- rep(NA, times = n_participants)
m2_w_evidence_within_90ci    <- rep(NA, times = n_participants)
m2_w_prior_sr_within_50ci    <- rep(NA, times = n_participants)
m2_w_prior_sr_within_90ci    <- rep(NA, times = n_participants)
m2_w_evidence_sr_within_50ci <- rep(NA, times = n_participants)
m2_w_evidence_sr_within_90ci <- rep(NA, times = n_participants)
m2_delta_w_prior_within_50ci <- rep(NA, times = n_participants) ## I really need a shorter variable name for this.
m2_delta_w_prior_within_90ci <- rep(NA, times = n_participants)


## Sampling
if(run_model_flag){
  tic()
  m2_fit <- compiled_model$sample(
    data            = stan_data,
    chains          = 4,
    parallel_chains = 4,
    refresh         = 100,
    iter_warmup     = 500,
    iter_sampling   = 1000,
    save_warmup     = FALSE
  )
  toc()
  
  ## Play audio cue
  beepr::beep("fanfare")
  
  ## Print and/or save samples
  m2_fit$save_output_files(
    dir      = here::here("output"),
    basename = model_to_fit$model_name
  )
}

# Extract variables ----------------

output_files <-  m2_fit$output_files()

## extract log-likelihood matrix
m2_log_likelihood <- read_cmdstan_csv(
  files               = output_files,
  variables           = c("choice_log_lik"),
  sampler_diagnostics = NULL,
  format              = getOption("cmdstanr_draws_format", "draws_df")
)

## extract predicted choices
m2_choice_pred <- read_cmdstan_csv(
  files               = output_files,
  variables           = c("choice_pred"),
  sampler_diagnostics = NULL,
  format              = getOption("cmdstanr_draws_format", "draws_df")
)

## get WAIC for model
m2_ll_samples <- as.matrix(
  m2_log_likelihood$post_warmup_draws[, 1:m2_log_likelihood$metadata$stan_variable_sizes$choice_log_lik,]
)
m2_model_WAIC <- loo::waic(m2_ll_samples)


#####################################################
## Extract parameter samples (Group-level)
#####################################################

## Extract m2 group parameter estimates
m2_group_par_samples_all <- read_cmdstan_csv(
  files = output_files,
  variables = model_to_fit$group_pars,
  sampler_diagnostics = NULL,
  format = getOption("cmdstanr_draws_format", "draws_df")
)

## Get median group parameter estimates
m2_group_par_samples <- as.matrix(m2_group_par_samples_all$post_warmup_draws[,1:length(model_to_fit$group_pars)])
m2_group_par_est     <- apply(m2_group_par_samples, MARGIN=2, FUN = median)
m2_group_par_CI      <- apply(m2_group_par_samples, MARGIN = 2, FUN = quantile , probs = c(.025, .5, .975))


## Get quantiles (credible intervals)
apply(m2_group_par_samples, MARGIN=2, FUN=quantile, c(.025, .5, .975))
quantile(sim_data$true_w_prior, c(.025, .5, .975))


## Extract model choices
m2_pred_right_prop <- colMeans(m2_choice_pred$post_warmup_draws)[1:(40*150*2)]


#####################################################
## Extract parameter samples (Individual-level)
#####################################################

## Extract parameter samples (m2_individual-level)
m2_indiv_par_samples_all <- read_cmdstan_csv(
  files= output_files,
  variables = model_to_fit$indiv_pars,
  sampler_diagnostics = NULL,
  format = getOption("cmdstanr_draws_format", "draws_df")
)

m2_indiv_par_samples <- vector(mode="list",
                               length=length(model_to_fit$indiv_pars))


m2_indiv_par_est <- matrix(NA, nrow = m2_indiv_par_samples_all$metadata$stan_variable_sizes[[model_to_fit$indiv_pars[1]]],
                           ncol = length(model_to_fit$indiv_pars))
colnames(m2_indiv_par_est) <- model_to_fit$indiv_pars

## Plot distribution of participant parameters
for (i in 1:length(m2_indiv_par_samples)){
  m2_indiv_par_samples[[i]] <-
    as.matrix(m2_indiv_par_samples_all$post_warmup_draws[seq(
      from       = 1 + (i-1) * dim(m2_indiv_par_est)[1],
      to         = i * dim(m2_indiv_par_est)[1],
      length.out = dim(m2_indiv_par_est)[1])
    ])
  m2_indiv_par_est[,i] <- apply(m2_indiv_par_samples[[i]], MARGIN=2, FUN=median)
  
  hist(m2_indiv_par_est[,i], main=model_to_fit$indiv_pars[i], 30)
}

## Code to get beta sample distribution for e.g., participant no. 11
pt_id <- 11
pt_ix <- match(pt_id, sim_data$PtID %>% unique())
param <- match("beta", model_bayes_precis_pp_offset$indiv_pars)  
hist(m2_indiv_par_samples[[param]][, pt_id], 30, main= paste("beta weight Samples for", pt_id))

## Record median parameter values for each participant
for (pt_ix in 1:n_participants){
  m2_est_beta[pt_ix]          <- m2_indiv_par_samples[[1]][,pt_ix] %>% median()
  m2_est_w_prior[pt_ix]       <- m2_indiv_par_samples[[2]][,pt_ix] %>% median()
  m2_est_w_evidence[pt_ix]    <- m2_indiv_par_samples[[3]][,pt_ix] %>% median()
  m2_est_w_prior_sr[pt_ix]    <- m2_indiv_par_samples[[4]][,pt_ix] %>% median()
  m2_est_w_evidence_sr[pt_ix] <- m2_indiv_par_samples[[5]][,pt_ix] %>% median()
  m2_est_delta_w_prior[pt_ix] <- m2_indiv_par_samples[[6]][,pt_ix] %>% median()
  
  m2_beta_within_50ci[pt_ix] <- m2_est_beta[pt_ix] > quantile(m2_indiv_par_samples[[1]], 0.25) & m2_est_beta[pt_ix] < quantile(m2_indiv_par_samples[[1]], 0.75)
  m2_beta_within_90ci[pt_ix] <- m2_est_beta[pt_ix] > quantile(m2_indiv_par_samples[[1]], 0.05) & m2_est_beta[pt_ix] < quantile(m2_indiv_par_samples[[1]], 0.95)
  
  m2_w_prior_within_50ci[pt_ix] <- m2_est_w_prior[pt_ix] > quantile(m2_indiv_par_samples[[2]], 0.25) & m2_est_w_prior[pt_ix] < quantile(m2_indiv_par_samples[[2]], 0.75)
  m2_w_prior_within_90ci[pt_ix] <- m2_est_w_prior[pt_ix] > quantile(m2_indiv_par_samples[[2]], 0.05) & m2_est_w_prior[pt_ix] < quantile(m2_indiv_par_samples[[2]], 0.95)
  
  m2_w_evidence_within_50ci[pt_ix] <- m2_est_w_evidence[pt_ix] > quantile(m2_indiv_par_samples[[3]], 0.25) & m2_est_w_evidence[pt_ix] < quantile(m2_indiv_par_samples[[3]], 0.75)
  m2_w_evidence_within_90ci[pt_ix] <- m2_est_w_evidence[pt_ix] > quantile(m2_indiv_par_samples[[3]], 0.05) & m2_est_w_evidence[pt_ix] < quantile(m2_indiv_par_samples[[3]], 0.95)
  
  m2_w_prior_sr_within_50ci[pt_ix] <- m2_est_w_prior_sr[pt_ix] > quantile(m2_indiv_par_samples[[4]], 0.25) & m2_est_w_prior_sr[pt_ix] < quantile(m2_indiv_par_samples[[4]], 0.75)
  m2_w_prior_sr_within_90ci[pt_ix] <- m2_est_w_prior_sr[pt_ix] > quantile(m2_indiv_par_samples[[4]], 0.05) & m2_est_w_prior_sr[pt_ix] < quantile(m2_indiv_par_samples[[4]], 0.95)
  
  m2_w_evidence_sr_within_50ci[pt_ix] <- m2_est_w_evidence_sr[pt_ix] > quantile(m2_indiv_par_samples[[5]], 0.25) & m2_est_w_evidence_sr[pt_ix] < quantile(m2_indiv_par_samples[[5]], 0.75)
  m2_w_evidence_sr_within_90ci[pt_ix] <- m2_est_w_evidence_sr[pt_ix] > quantile(m2_indiv_par_samples[[5]], 0.05) & m2_est_w_evidence_sr[pt_ix] < quantile(m2_indiv_par_samples[[5]], 0.95)
  
  m2_delta_w_prior_within_50ci[pt_ix] <- m2_est_delta_w_prior[pt_ix] > quantile(m2_indiv_par_samples[[6]], 0.25) & m2_est_delta_w_prior[pt_ix] < quantile(m2_indiv_par_samples[[6]], 0.75)
  m2_delta_w_prior_within_90ci[pt_ix] <- m2_est_delta_w_prior[pt_ix] > quantile(m2_indiv_par_samples[[6]], 0.05) & m2_est_delta_w_prior[pt_ix] < quantile(m2_indiv_par_samples[[6]], 0.95)
  
}

## Check calibration
mean(m2_beta_within_50ci); mean(m2_beta_within_90ci)
mean(m2_w_prior_within_50ci); mean(m2_w_prior_within_90ci)
mean(m2_w_evidence_within_50ci); mean(m2_w_evidence_within_90ci)
mean(m2_w_prior_sr_within_50ci); mean(m2_w_prior_sr_within_90ci)
mean(m2_w_evidence_sr_within_50ci); mean(m2_w_evidence_sr_within_90ci)
mean(m2_delta_w_prior_within_50ci); mean(m2_delta_w_prior_within_90ci)


# m2: save model objects --------------------------------------------------

save(list = ls(pattern = "m2_"), file = here::here("output", "model-Rdata", "m2_model_objects.Rdata"))


# Model comparison --------------------------------------------------------

loo::loo_compare(m1_model_WAIC, m2_model_WAIC)

## ======================
