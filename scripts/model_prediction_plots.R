# Load packages
require(ggplot2)
require(plotrix)
require(hrbrthemes)


# =========================================================================
# Model 1 Q-Learning ------------------------------------------------------
# =========================================================================


## Create tally datatable
m1_tally_df <- cbind(sim_data[, .(subject, condition, trial_no, ptresp, cresp, outcome, trials_since_reversal)], m1_pred_right_prop) %>% as.data.table()

## Create model ptresp column based on predicted right probability
m1_tally_df[,model_ptresp:=ifelse(m1_pred_right_prop > .50, 1, 0)]

## Tally correct responses
m1_tally_df[,pt_correct:=ifelse(ptresp==cresp, 1, 0)]
m1_tally_df[,model_correct:=ifelse(model_ptresp==cresp, 1, 0)]

## Mean and SD for participants by time since switch
m1_pt_tally_pre <- m1_tally_df[condition == "pre", .(subject, condition, trial_no, cresp, pt_correct, trials_since_reversal)]
m1_pt_stats_pre <- m1_pt_tally_pre[, .(mean(pt_correct), sd(pt_correct), std.error(pt_correct)), by = trials_since_reversal]
m1_pt_stats_pre[, "ptresp_by":= "pt"]
m1_pt_stats_pre[, "condition":= "pre"]
m1_pt_tally_post <- m1_tally_df[condition == "post", .(subject, condition, trial_no, cresp, pt_correct, trials_since_reversal)]
m1_pt_stats_post <- m1_pt_tally_post[, .(mean(pt_correct), sd(pt_correct), std.error(pt_correct)), by = trials_since_reversal]
m1_pt_stats_post[, "ptresp_by":= "pt"]
m1_pt_stats_post[, "condition":= "post"]

## Mean and SD for model 1 by time since switch
m1_model_tally_pre <- m1_tally_df[condition == "pre", .(subject, condition, trial_no, model_correct, trials_since_reversal)]
m1_model_stats_pre <- m1_model_tally_pre[, .(mean(model_correct), sd(model_correct), std.error(model_correct)), by = trials_since_reversal]
m1_model_stats_pre[, "ptresp_by":= "mod"]
m1_model_stats_pre[, "condition":= "pre"]
m1_model_tally_post <- m1_tally_df[condition == "post", .(subject, condition, trial_no, model_correct, trials_since_reversal)]
m1_model_stats_post <- m1_model_tally_post[, .(mean(model_correct), sd(model_correct), std.error(model_correct)), by = trials_since_reversal]
m1_model_stats_post[, "ptresp_by":= "mod"]
m1_model_stats_post[, "condition":= "post"]

## Merge stats
m1_performance <- rbind(m1_pt_stats_pre, m1_pt_stats_post, m1_model_stats_pre, m1_model_stats_post)
names(m1_performance)[2:4] <- c("mean_correct", "sd_correct", "se_correct")
rm(m1_pt_stats_pre); rm(m1_model_stats_pre); rm(m1_pt_stats_post); rm(m1_model_stats_post)

## Compute upper and lower bounds for SE
m1_performance[, c("se.upper", "se.lower"):= .(mean_correct + se_correct, mean_correct-se_correct)]
m1_performance[, c("sd.upper", "sd.lower"):= .(mean_correct + sd_correct, mean_correct-sd_correct)]
m1_performance[, c("ci.upper", "ci.lower"):= .(mean_correct + (1.96 * se_correct), mean_correct - (1.96 * se_correct))]

m1_performance[, plotcat := ifelse(condition == "pre" & ptresp_by == "pt", 1, 
                                   ifelse(condition == "pre" & ptresp_by == "mod", 2,
                                          ifelse(condition == "post" & ptresp_by =="pt", 3, 4)))]
m1_performance[,plotcat := as.character(plotcat)]

## Factor condition so conditions show up in right order during plotting
m1_performance[,condition:=factor(condition, levels = c("pre", "post"))]


# Model v Participant: Prop of ptresps correct ----------------------------

colors <- c("#617a89", ft_cols$red)

m1_pre_plot <- ggplot(m1_performance[condition == "pre"], aes(x = trials_since_reversal, y = mean_correct, color = ptresp_by, fill = ptresp_by)) + 
  geom_line(linewidth =1.2, alpha = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_ribbon(aes(ymin = ci.lower, ymax = ci.upper, color = NULL), alpha = .201 ) + 
  scale_color_manual(values = colors, labels = c("Model", "Participant")) +
  scale_fill_manual(values = c("#617a89", ft_cols$red), labels = c("Model", "Participant")) +
  theme_ipsum_rc() +
  ylim(0, 1) + 
  theme(axis.title.x=element_text(size = 16), axis.title.y = element_text(size = 16)) + 
  labs(title = "Proportion of correct choices during 'pre' (Simulated)", subtitle = "Model versus 'Participant'", 
       x = "Time since reversal", y = "Proportion of choices correct", color = "Choices by", fill = "Choices by") 

m1_post_plot <- ggplot(m1_performance[condition == "post"], aes(x = trials_since_reversal, y = mean_correct, color = ptresp_by, fill = ptresp_by)) + 
  geom_line(linewidth =1.2, alpha = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) + 
  geom_ribbon(aes(ymin = ci.lower, ymax = ci.upper, color = NULL), alpha = .201 ) + 
  scale_color_manual(values = colors, labels = c("Model", "Participant")) +
  scale_fill_manual(values = c("#617a89", ft_cols$red), labels = c("Model", "Participant")) +
  theme_ipsum_rc() +
  ylim(0, 1) + 
  theme(axis.title.x=element_text(size = 16), axis.title.y = element_text(size = 16)) + 
  labs(title = "Proportion of correct choices during 'post' (Simulated)", subtitle = "Model versus 'Participant'", 
       x = "Time since reversal", y = "Proportion of choices correct", color = "Choices by", fill = "Choices by") 


m1_plot + facet_grid(cols = vars(condition))


## Proportion matched plots -----------------------------------

### Wrangle data and tally matches
model_predictions <- m1_choice_pred$post_warmup_draws[1:16000] %>% colMeans()
sim_data[,m1_pred_prob := model_predictions]
sim_data[,m1_ptresp := ifelse(m1_pred_prob > .5, 1, 0)]

sim_data[,m1_model_pt_match:= ifelse(m1_ptresp == ptresp, 1, 0)]


m1_prediction_tally <- sim_data[, .(mean(m1_model_pt_match), std.error(m1_model_pt_match)), by = subject]
names(m1_prediction_tally) <- c("subject", "mean_correct", "se_correct")
m1_prediction_tally[,c("ci.upper", "ci.lower") := .(mean_correct + (1.96*se_correct), mean_correct - (1.96*se_correct))]

### Rename subject column so that the plot is in order
for (subj_x in m1_prediction_tally$subject){
  if (subj_x < 10){
    plot_ID <- paste0("Subject 0", as.character(subj_x))
  } else {
    plot_ID <- paste0("Subject ", subj_x)
  }
  m1_prediction_tally[subject == subj_x, "plot_ID"] <- plot_ID
}

### Plot data

ggplot(m1_prediction_tally, aes(x = mean_correct, y = plot_ID, xmin = ci.lower, xmax = ci.upper, color = mean_correct)) +
  scale_x_continuous(breaks = seq(0.1, 1, by = .1), limits = c(.2, 1))+ 
  geom_errorbarh(height = .2) + geom_point(size = 4) + 
  geom_vline(xintercept = .5, linetype = "dashed")+
  annotate("text", x = .475, y = "Subject 22", label = "Chance level", angle = 90, size = 5) +
  labs(title = "Proportion of Choices Matched by Model", x = "Proportion", y=  "Participant ID" )+
  theme_ipsum() +
  theme(axis.title.y = element_text(size = 16), axis.title.x = element_text(size = 16)) +
  scale_color_gradient(low = "#758bd1", high = "#3f2d54") + guides(color = FALSE)


## Estimated vs actual parameter plots -----------------------------------


## Plot estimated vs actual

## Bind est vs actual values into a datatable
actual_pars <- sim_data[, c('subject',"true_eta_pre", "true_delta_eta", "true_beta")]
actual_pars <- distinct(actual_pars, subject, .keep_all = T)
m1_est_v_actual_parameters <- cbind(actual_pars[, c("true_eta_pre", "true_delta_eta", "true_beta")],
                                             m1_est_eta, m1_est_delta_eta, m1_est_beta) %>% as.data.table()

## Beta correlations
beta_corr_plot <- ggplot(data = m1_est_v_actual_parameters, aes(x = true_beta, y = m1_est_beta)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", color = colors[1], fill = colors[1], alpha = .2, fullrange=T ) +
  theme_ipsum() +
  labs(title = expression("β"),  x = "Actual", y = "Model-estimated")+ 
  theme(axis.title.x =element_text(size = 16), axis.title.y = element_text(size = 16))+
  ylim(0, 20)

## W_prior correlations
eta_pre_corr_plot <- ggplot(data = m1_est_v_actual_parameters, aes(x = true_eta_pre, y = m1_est_eta)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", color = "#ff7300", fill = "#ff7300",  alpha = .2, fullrange = T  ) +
  theme_ipsum() +
  labs(title = expression("η"[pre]),  x = "Actual", y = "Model-estimated") + 
  theme(axis.title.x =element_text(size = 16), axis.title.y = element_text(size = 16))+
  ylim(0, 1) + xlim(0, 1)


## Δ η prior correlations
delta_corr_plot<-ggplot(data = m1_est_v_actual_parameters, aes(x = true_delta_eta, y = m1_est_delta_eta )) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", color = "#444791", fill = "#444791", alpha = .2, fullrange = T ) +
  theme_ipsum() +
  labs(title = expression(Δ[η[prior]]), x = "Actual", y = "Model-estimated") +
  theme(axis.title.x =element_text(size = 16), axis.title.y = element_text(size = 16)) + 
  ylim(-1.5, 2) + xlim(-1.5, 2)

## Put all plots together
mod_v_actual_plots <- ggpubr::ggarrange(beta_corr_plot, eta_pre_corr_plot, delta_corr_plot, ncol = 2, nrow = 2)
ggpubr::annotate_figure(mod_v_actual_plots, top = ggpubr::text_grob("Model-estimated vs actual parameter values", 
                                                    face = "bold", size = 16, vjust = 2.7, hjust = -.1 , x = 0))
