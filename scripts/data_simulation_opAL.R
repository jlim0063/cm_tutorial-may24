# Load packages
require(dplyr)
require(data.table)

# Define task parameters --------------------------------------------------

n_pt         <- 40  # Number of participants
n_trials     <- 200 # Number of trials
n_conditions <- 2  # Number of experimental conditions
amt_reward   <- 1   # Reward amount on each trial
amt_penalty  <- 0   # Penalty amount on each trial

pr_reversal        <- .10                    # Probability of reversal on each new trial
pr_win_good_option <- .7                     # Probability of reward if chose good option
pr_win_bad_option  <- 1 - pr_win_good_option # Probability of reward if chose bad option

min_trials_since_reversal <- 5   # We artificially set the minimum trials before a reversal can occur
max_trials_since_reversal <- 25  # And the maximum number of trials since reversal that is allowable

# Generate trials -----------------------------------------------------------

## Create container for all trials for all participants
subject     <- rep(1:n_pt, each = n_trials * n_conditions)          # Participant id  
condition   <- rep(c("pre", "post"), times = n_pt, each = n_trials) # Experimental treatment conditions
trial_no    <- rep(1:n_trials, times = n_pt * n_conditions)         # Trial number
cresp       <- rep(0, n_pt * n_trials * n_conditions)               # Good option on trial t (cresp = correct response)
reward      <- rep(0, n_pt * n_trials * n_conditions)               # Actual feedback on trial t (reward or penalty amount)
trials_since_reversal <- rep(0, n_pt * n_trials * n_conditions)     # Number of trials since last reversal occurred
reversal_event  <- rep(0, n_pt * n_trials * n_conditions)           # Binary indicator of whether a reversal has occurred on the current trial
win_if_choose_a <- rep(0, n_pt * n_trials * n_conditions)           # Amount to be rewarded if chosen a
win_if_choose_b <- rep(0, n_pt * n_trials * n_conditions)           # Amount to be rewarded if chosen b      


## Bind all the above into one data table
sim_data <- data.table(subject, condition, trial_no, cresp, reward, trials_since_reversal, reversal_event)

## Now, we generate all trial contingencies
## i.e., which are the good options, bad options, and whether pts get rewarded for selecting each option
## Because we are generating contingencies trial by trial, this code chunk may take a while
set.seed(123)

for (subject_x in 1:1) {
# for (subject_x in 1:n_pt) {
  # Print visual confirmation
  message(paste("Generating trial contingencies for Subject ID ", subject_x))
  
  for (cond_x in unique(condition)){
    # Print visual confirmation
    message(paste("Condition:", cond_x))
    
    for (trial_t in 1:n_trials){
      # Print visual confirmation
      message(paste0("Trial:",trial_t ))
      flush.console()
      n <- nchar(paste0("Trial:", trial_t)) + 1
      cat(paste(rep("\b", n), collapse = ""))
      
      if (trial_t == n_trials){
        flush.console()
        message("Complete!")
      }
      
      # Start sampling trials
      if (trial_t == 1){
        # Print visual confirmation
        message(paste0("Trial:",trial_t ))
        flush.console()
        n <- nchar(paste0("Trial:", trial_t)) + 1
        cat(paste(rep("\b", n), collapse = ""))
        
        if (trial_t == n_trials){
          flush.console()
          message("Complete!")
        }
        
        # If trial is the first one, pick the good option at chance level
        good_option <- sample(0:1, prob = (c(0.5, 0.5)), size = 1)
        
        # and write the good option to sim_data
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "cresp"] <- good_option
      } else {
        
        ## If trial is not the first one, 
        ## we update the counter from the last trial
        n_trials_since_reversal <- sim_data[subject==subject_x & condition == cond_x & trial_no == trial_t-1, trials_since_reversal] + 1
        
        ## we sample for whether a reversal should happen
        ## But we only do it if it has been >5 trials AND <= 25 trials since reversals 
        if (n_trials_since_reversal <= min_trials_since_reversal){
          reversal <- 0
        } else if (n_trials_since_reversal > max_trials_since_reversal){
          reversal <- 1
        } else {
          reversal <- sample(0:1, prob = c(1-pr_reversal, pr_reversal), size = 1)
        }
        
        ## If a reversal should occur on the current trial, we reset the number of trials since reversal to 0
        if (reversal == 1){
          n_trials_since_reversal <- 0
        } 
        
        ## We grab the good option from the previous trial
        good_option      <- sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t - 1, cresp]
        
        ## Create a temporary container of available options - in our case, only 2. 
        possible_options <- 0:1
        
        ## And we swap out the good option if a reversal should occur
        good_option <- ifelse(reversal == 1, sample(possible_options[!possible_options %in% good_option], size = 1), good_option)
        
        ## Write all relevant data to sim_data
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "cresp"]                 <- good_option
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "trials_since_reversal"] <- n_trials_since_reversal
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "reversal_event"]        <- reversal
        
      }
      
      ## Next, we need to determine, according to the good option, whether a participant gets rewarded or not
      ## Remember that it is not certain they will be rewarded, even if they chose the good option
      if (good_option == 0){
        reward_a <- sample(c(amt_reward, amt_penalty), size = 1, prob = c(pr_win_good_option, pr_win_bad_option))
        reward_b <- sample(c(amt_reward, amt_penalty), size = 1, prob = c(pr_win_bad_option, pr_win_good_option))
      } else{
        reward_a <- sample(c(amt_reward, amt_penalty), size = 1, prob = c(pr_win_bad_option, pr_win_good_option))
        reward_b <- sample(c(amt_reward, amt_penalty), size = 1, prob = c(pr_win_good_option, pr_win_bad_option))
      }
      
      ## Write the possible rewards to sim_data
      sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "win_if_choose_a"] <- reward_a
      sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "win_if_choose_b"] <- reward_b
    }
  }
}


# Simulate participant choices --------------------------------------------

set.seed(123)
## Sample participant parameters
eta_G_list_raw <- rnorm(n_pt, mean = 0, sd = 1)
eta_N_list_raw <- rnorm(n_pt, mean = 0, sd = 1)

delta_eta_G_list_raw <- rnorm(n_pt, mean = -.4, sd = 1)
delta_eta_N_list_raw <- rnorm(n_pt, mean = -.4, sd = 1)

eta_G_list     <- pnorm(eta_G_list_raw)
eta_N_list     <- pnorm(eta_N_list_raw)

eta_G_post_list <- pnorm(eta_G_list_raw + delta_eta_G_list_raw)
eta_N_post_list <- pnorm(eta_N_list_raw + delta_eta_N_list_raw)

beta_G_list       <- pnorm(rnorm(n_pt, mean = 0, sd = 1)) * 2
beta_NG_list      <- pnorm(rnorm(n_pt, mean = 0, sd = 1)) * 2

## Write true parameter values to sim_data
for (subject_x in unique(sim_data$subject)){
  sim_data[subject == subject_x, "true_eta_G_pre"]    <- eta_G_list[subject_x]
  sim_data[subject == subject_x, "true_eta_N_pre"]    <- eta_N_list[subject_x]
  sim_data[subject == subject_x, "true_delta_eta_G"]  <- delta_eta_G_list_raw[subject_x]
  sim_data[subject == subject_x, "true_delta_eta_N"]  <- delta_eta_N_list_raw[subject_x]
  sim_data[subject == subject_x, "true_eta_G_post"]   <- eta_G_post_list[subject_x]
  sim_data[subject == subject_x, "true_eta_N_post"]   <- eta_N_post_list[subject_x]
  sim_data[subject == subject_x, "true_beta_G"]       <- beta_G_list[subject_x]
  sim_data[subject == subject_x, "true_beta_NG"]      <- beta_NG_list[subject_x]
}

## Create columns to hold choices, Q values and probability values
sim_data[, ptresp := as.double()]        # Participant choice
sim_data[, pt_G_A := as.double()]        # Participant's subjective Q value associated with stimulus A
sim_data[, pt_G_B := as.double()]        # Participant's subjective Q value associated with stimulus B
sim_data[, pt_NG_A := as.double()]       # Participant's subjective Q value associated with stimulus A
sim_data[, pt_NG_B := as.double()]       # Participant's subjective Q value associated with stimulus B
sim_data[, pr_choose_a := as.double()]   # Participant's probability of choosing A (after applying softmax function)
sim_data[, outcome:=as.double()]         # Outcome of participant's choice

# for (subject_x in 1:1){
for (subject_x in unique(sim_data$subject)){
  # Print visual confirmation
  message(paste("Simulating choices for Subject ID:", subject_x))
  for (cond_x in unique(sim_data$condition)){
    # Print visual confirmation
    message(paste("Condition:", cond_x))
    
    for (trial_t in 1:n_trials){
      # Print visual confirmation
      message(paste0("Trial:",trial_t ))
      flush.console()
      n <- nchar(paste0("Trial:", trial_t)) + 1
      cat(paste(rep("\b", n), collapse = ""))
      
      if (trial_t == n_trials){
        flush.console()
        message("Complete!")
      }
      if (trial_t == 1){
        ## If it is trial 1, participant will randomly pick either stimulus at chance level
        pr_choose_a <- 1/2 
        pt_choice   <- sample(0:1, size = 1, prob = c(.5, .5))
        
        ## Their Q values for trial #1 is equivalent to the summed weighted possible outcomes, with each outcome weighted by the probability of being the good/bad choice
        G_A  <-  amt_reward * .5 + amt_penalty * .5
        G_B  <-  amt_reward * .5 + amt_penalty * .5
        NG_A <-  amt_reward * .5 + amt_penalty * .5
        NG_B <-  amt_reward * .5 + amt_penalty * .5
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "pt_G_A"]  <- G_A
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "pt_G_B"]  <- G_B
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "pt_NG_A"] <- NG_A
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "pt_NG_B"] <- NG_B
      } else{
        
        ## If not trial 1, then use Q values to determine probability of picking A
        G_A  <- sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, pt_G_A]
        G_B  <- sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, pt_G_B] 
        NG_A <- sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, pt_NG_A]
        NG_B <- sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, pt_NG_B] 
      }
      
      ## Test case for NG values
      # if (NG_A > .75){
      #   NG_A <- 3
      # }
      # if (NG_B > .75){
      #   NG_B <- 3
      # }
      
      # Extract effective beta values for each participant
      beta_G_effective  <- unique(sim_data[subject == subject_x, "true_beta_G"]) %>% as.numeric()
      beta_NG_effective <- unique(sim_data[subject == subject_x, "true_beta_NG"]) %>% as.numeric()
      
      # Calculate actor weights from G and NG values
      act_a <- beta_G_effective*G_A - beta_NG_effective*NG_A
      act_b <- beta_G_effective*G_B - beta_NG_effective*NG_B
      
      # Softmax to determine probability of choosing A
      pr_choose_a <- exp(act_a)/sum(c(exp(act_a), exp(act_b)))
      
      # Sample participant choice 
      pt_choice <- sample(0:1, size = 1, prob = c(pr_choose_a, 1-pr_choose_a))
      

      ## We already have the rewards associated with each choice written into sim_data, 
      ## so we can pick out the outcome that corresponds with their choice
      outcome <- ifelse(
        pt_choice == 0, 
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, win_if_choose_a], 
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, win_if_choose_b]
      )
      
      ## Then, we pick out the effective eta values according to condition "pre" or "post"
      eta_G_effective <- ifelse(cond_x == "pre", 
                                unique(sim_data[subject==subject_x]$true_eta_G_pre),
                                unique(sim_data[subject==subject_x]$true_eta_G_post)
      )
      eta_NG_effective <- ifelse(cond_x == "pre", 
                                unique(sim_data[subject==subject_x]$true_eta_N_pre),
                                unique(sim_data[subject==subject_x]$true_eta_N_post)
      )

      ## and update G and NG values for the next trial                
      if (pt_choice == 0){
        G_A_next  <- G_A + (eta_G_effective*G_A)*(outcome-G_A)
        G_B_next  <- G_B
        NG_A_next <- NG_A + (eta_NG_effective*NG_A)*(outcome-NG_A)
        NG_B_next <- NG_B
      } else {
        G_A_next  <- G_A
        G_B_next  <- G_B + (eta_G_effective*G_B)*(outcome-G_B)
        NG_A_next <- NG_A 
        NG_B_next <- NG_B + (eta_G_effective*NG_B)*(outcome-NG_B)
      }
      
      ## Write all the relevant information into sim_data
      sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "pr_choose_a"] <- pr_choose_a
      sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "ptresp"]      <- pt_choice
      sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t, "outcome"]     <- outcome

      ## We also record the G and NG values for the next trial, but only if the current trial t isn't the last trial
      if (trial_t != n_trials){
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t+1, "pt_G_A"]    <- G_A_next
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t+1, "pt_G_B"]    <- G_B_next
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t+1, "pt_NG_A"]   <- NG_A_next
        sim_data[subject == subject_x & condition == cond_x & trial_no == trial_t+1, "pt_NG_B"]   <- NG_B_next
      }
    }
  }
}
beepr::beep(sound = "coin")

# Export simulated data to csv --------------------------------------------------

write.csv(sim_data, file = here::here("data", "simulated_data_opAL.csv"))




