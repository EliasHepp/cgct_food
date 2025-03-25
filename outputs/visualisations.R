###Load packages

p_required <- c("dplyr", "ggplot2", "tidyr", "patchwork", "rnaturalearthdata", "rnaturalearth", "terra") #package names
packages <- rownames(installed.packages()) #vector with all existing packages
p_to_install <- p_required[!(p_required %in% packages)] #vector with all packages that are in p_required, but not                                                         packages
if (length(p_to_install) > 0) { 
  install.packages(p_to_install)
}#install all packages in p_to_install                                                 
sapply(p_required, require, character.only = TRUE)#make sure all objects only in character
rm(p_required, p_to_install, packages) #remove now unnecessary vectors

rm(list = ls())


###################
##Model Performance
###################

#visualisation function
plot_perf <- function(input_table, models_sel, model_names, y_cap = FALSE, title, y_des){
  mean_values <- input_table[input_table$measure == "mean", -1]
  sd_values <- input_table[input_table$measure == "sd", -1]  
  
  mean_long <- pivot_longer(mean_values, cols = everything(), names_to = "Model", values_to = "Mean")
  sd_long <- pivot_longer(sd_values, cols = everything(), names_to = "Model", values_to = "SD")
  
  model_mapping <- setNames(model_names, models_sel)
  
  mean_long$Model <- factor(mean_long$Model, levels = models_sel)
  mean_long$Model <- recode(mean_long$Model, !!!model_mapping)
  
  sd_long$Model <- factor(sd_long$Model, levels = models_sel)
  sd_long$Model <- recode(sd_long$Model, !!!model_mapping)
  
  plot_dat <- merge(mean_long, sd_long, by = "Model")
  
  plot_dat <- plot_dat %>% filter(Model %in% model_names)
  plot_dat$Model <- factor(plot_dat$Model, levels = model_names)
  
  if(y_cap != FALSE){
    plot_dat$Mean <- pmin(plot_dat$Mean, y_cap)
  }
  else{
    max(plot_dat$Mean + plot_dat$SD)
  }
  
  best_model <- plot_dat$Model[which.min(plot_dat$Mean)]
  
  plot <- ggplot(plot_dat, aes(x = Model, y = Mean, fill = Model)) +
    geom_bar(stat = "identity", color = "black", aes(fill = ifelse(Model == best_model, "best", "other")), alpha = 0.5) +  
    geom_errorbar(aes(ymin = pmax(Mean - SD, 0), ymax = pmin(Mean + SD, y_cap)), width = 0.2, color = "black") +  
    ylim(0, y_cap) +  
    theme_minimal() +
    labs(title = title,
         y = y_des,
         x = "Model") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    scale_fill_manual(values = c("best" = "#005555", "other" = "black"))  # best model red
  
  return(plot)
}


#Real World - Small Dataset

model_perf_real <- read.csv("model_perf_real.csv", row.names = 1)
model_order <- c("lm", "nn", "rf", "cgct_rf", "dr", "sci", "gps", "cgct_gps")
model_names <- c("LM", "NN", "RF", "CG-CT RF", "DR", "SCI", "GPS", "CG-CT GPS")

plot_perf_real <- plot_perf(model_perf_real, model_order, model_names, 1, "Real Data", "RMSE")
ggsave("plot_perf_real.png", plot = plot_perf_real, width = 8, height = 6, dpi = 300)

#Real World - Large Dataset
model_perf_real_large <- read.csv("model_perf_real_large.csv", row.names = 1)
model_order <- c("lm", "nn", "rf", "cgct_rf", "dr", "sci", "gps", "cgct_gps")
model_names <- c("LM", "NN", "RF", "CG-CT RF", "DR", "SCI", "GPS", "CG-CT GPS")

plot_perf_real_large <- plot_perf(model_perf_real_large, model_order, model_names, 1, "", "RMSE")
ggsave("model_perf_real_large.png", plot = plot_perf_real_large, width = 8, height = 6, dpi = 300)

#Simulated data - Large Dataset
model_perf_sim <- read.csv("model_perf_sim.csv", row.names = 1)
model_order <- c("lm", "nn", "rf", "cgct_rf", "dr", "sci", "gps", "cgct_gps")
model_names <- c("LM", "NN", "RF", "CG-CT RF", "DR", "SCI", "GPS", "CG-CT GPS")

plot_perf_sim <- plot_perf(model_perf_sim, model_order, model_names, 1, "Synthetic Data", "RMISE")
ggsave("plot_perf_sim.png", plot = plot_perf_sim, width = 8, height = 6, dpi = 300)


model_perf_plots <- (plot_perf_real | plot_perf_sim) +  
  plot_layout(guides = "collect") 

ggsave("model_res_plots.png", plot = model_perf_plots, width = 16, height = 8, dpi = 300)


#######################
##Optimization Analysis
#######################



optim_all_aid <- read.csv("optim_allocation_boots.csv")
ci_obs <- quantile(optim_all_aid$Y_obs_pred, probs = c(0.025, 0.975))
ci_pred <- quantile(optim_all_aid$Y_opt_pred, probs = c(0.025, 0.975))

optim_obs <- 757.9362848919659
optim_pred <- 750.7205151125045

optim_pred_unbound <- 742.1599991091456
optim_actual <- 743.8146950500001

alloc_df <- data.frame(
  category = c("Optimal Allocation", "Predicted Allocation", "Optimal Allocation Unbound", "Actual Allocation"),
  value = c(optim_obs, optim_pred, optim_pred_unbound, optim_actual),
  lower_ci = c(ci_obs[1], ci_pred[1], NA, NA),  # Only for first two
  upper_ci = c(ci_obs[2], ci_pred[2], NA, NA) # Only for first two
)
alloc_df$category <- factor(alloc_df$category, levels = c("Actual Allocation", "Optimal Allocation", "Predicted Allocation", "Optimal Allocation Unbound"))


# Plot
plot_optim_allocation <- ggplot(alloc_df, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", alpha = 0.5) +  
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "black") +  
  labs(x = "Value", y = "Food Insecure Peope, Million", title = "") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("black", "#003c3c", "#005555", "#55002b"))  
ggsave("optim_allocation.png", plot = plot_optim_allocation, width = 16, height = 8, dpi = 300)

absolute_dff <- optim_pred - optim_obs
absolute_dff_unbound <- optim_pred_unbound - optim_obs


(optim_pred - optim_obs) / optim_obs
(optim_pred_unbound - optim_obs) / optim_obs
relative_dff <- optim_pred/optim_obs  

relative_dff_unbound <- optim_pred/optim_obs  


#allocation plot

optim_all_aid <- read.csv("optim_allocation_A.csv", row.names = 1)

optim_all_aid$diff <- optim_all_aid$A_opt - optim_all_aid$A_factual
world <- ne_countries(scale = "medium", returnclass = "sf")

map_plot_realloc <- world %>%
  left_join(optim_all_aid, by = c("name" = "country"))

plot_realloc_opt <- ggplot(map_plot_realloc) +
  geom_sf(aes(fill = diff), color = "gray", lwd = 0.1) + 
  scale_fill_gradient(low = "#550055", high = "#00a2a2", na.value = "lightgray")  +
  theme_minimal() +
  theme_bw() +
  labs(title = "", fill = "Rellacoated Aid, Million USD") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
plot_realloc_opt
ggsave("../outputs/plot_realloc_opt.png", plot = plot_realloc_opt, width = 6, height = 4, dpi = 300)



##################
##Ablation Studies
##################

reshape_ablation <- function(input_table){
  mean_values <- input_table[input_table$measure == "mean", -1]
  sd_values <- input_table[input_table$measure == "sd", -1]  
  mean_long <- pivot_longer(mean_values, cols = everything(), names_to = "Model", values_to = "Mean")
  sd_long <- pivot_longer(sd_values, cols = everything(), names_to = "Model", values_to = "SD")

  plot_dat <- merge(mean_long, sd_long, by = "Model")
  
  return(plot_dat)
}

ablation <- read.csv("ablation.csv", row.names = 1)
ablation <- reshape_ablation(ablation)
ablation_bae <- read.csv("ablation_bae.csv", row.names = 1)
ablation_bae <- reshape_ablation(ablation_bae)
ablation_cf <- read.csv("ablation_cf.csv", row.names = 1)
ablation_cf <- reshape_ablation(ablation_cf)

ablation_base <- reshape_ablation(model_perf_sim)
ablation_base <- ablation_base[ablation_base$Model != "sci",]

#ablation_df <- rbind(ablation_base, ablation, ablation_bae, ablation_cf)
ablation_df <- rbind(ablation, ablation_bae, ablation_cf, ablation_base)
ablation_df$mod_type <- NA
mod_type <- c("lm", "nn", "dr", "gps", "rf")

for(i in mod_type){
  ablation_df$mod_type[grep(i, ablation_df$Model)] <- i
}

ablation_df <- ablation_df %>%
  mutate(category = case_when(
    grepl("nobae", Model) ~ "nobae",
    grepl("nocfgen", Model) ~ "nocf",
    grepl("cgct", Model) ~ "nocf",
    TRUE ~ "baseline"
  ))

ablation_df$mod_type <- c("DR", "LM", "NN", "DR", "GPS", "LM", "NN", "RF", "DR", "GPS", "LM", "NN", "RF", "GPS", "RF", "DR", "GPS", "LM", "NN", "RF")
ablation_df$mod_type <- factor(ablation_df$mod_type, levels = c("LM", "NN", "DR", "GPS", "RF"))
ablation_df$category <- c("CG-CT", "CG-CT", "CG-CT", "No BAE", "No BAE", "No BAE", "No BAE", "No BAE", "No CF", "No CF", "No CF", "No CF", "No CF", "CG-CT", "CG-CT", "Baseline", "Baseline", "Baseline", "Baseline", "Baseline")
ablation_df$category <- factor(ablation_df$category, levels = c("Baseline", "No BAE", "No CF", "CG-CT"))

# Plot
plot_ablation <- ggplot(ablation_df, aes(x = mod_type, y = Mean, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(title = "", x = "Model Type", y = "Mean RMISE", fill = "Model Specification") +
  theme_minimal() +
  theme_bw() +
  scale_fill_manual(values = c("Baseline" = "black", "No BAE" = "#002b55", "No CF" = "#005555", "CG-CT" = "#55002b"))
ggsave("ablation.png", plot = plot_ablation, width = 16, height = 8, dpi = 300)



###################
##Robustness Checks
################### 

#Performance Time Robustness

rob_yr_model_perf <- read.csv("rob_perf_years.csv", row.names = 1)
model_order <- c("lm", "nn", "rf", "cgct_rf", "dr", "sci", "gps", "cgct_gps")
model_names <- c("LM", "NN", "RF", "CG-CT RF", "DR", "SCI", "GPS", "CG-CT GPS")

plot_perf_rob <- plot_perf(rob_yr_model_perf, model_order, model_names, 1, "", "RMSE")
ggsave("rob_yr_model_perf.png", plot = plot_perf_rob, width = 8, height = 6, dpi = 300)

#CGCT Hyperpar Robustness

rob_alpha <- read.csv("rob_sens_param_alpha.csv", row.names = 1)
  
rob_alpha <- pivot_longer(rob_alpha, cols = everything(), names_to = "category", values_to = "value")
rob_alpha <- rob_alpha %>%
  group_by(category) %>%
  summarise(
    mean = mean(value),
    sd = sd(value)
  )
rob_alpha$category <- c("0.1", "1", "5")
rob_alpha$category_numeric <- as.numeric(as.character(rob_alpha$category))

ggplot(rob_alpha, aes(x = category_numeric, y = mean, group = 1, color = "black")) +
  geom_line(size = 1, color = "black") +  
  geom_point(size = 3, color = "black") +  
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +  
  labs(title = "", 
       x = expression(italic(theta)), 
       y = "RMISE") +
  theme_minimal() +
  scale_x_continuous(breaks = c(0.1, 1, 5), labels = c("0.1", "1", "5")) +  
  scale_y_continuous(limits = c(0, 0.4)) +  
  theme(legend.position = "none") 
ggsave("rob_alpha.png", width = 16, height = 8, dpi = 300)


rob_mscw <- read.csv("rob_sens_param_mscw.csv", row.names = 1)
rob_mscw <- pivot_longer(rob_mscw, cols = everything(), names_to = "category", values_to = "value")
rob_mscw <- rob_mscw %>%
  group_by(category) %>%
  summarise(
    mean = mean(value),
    sd = sd(value)
  )
rob_mscw$category <- c("1", "5", "7")
rob_mscw$category_numeric <- as.numeric(as.character(rob_mscw$category))

ggplot(rob_mscw, aes(x = category_numeric, y = mean, group = 1, color = "black")) +
  geom_line(size = 1, color = "black") +  
  geom_point(size = 3, color = "black") +  
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +  
  labs(title = "", 
       x = expression(italic("M")), 
       y = "RMISE") +
  theme_minimal() +
  scale_x_continuous(breaks = c(1, 5, 7), labels = c("1", "5", "7")) +  
  scale_y_continuous(limits = c(0, 0.4)) +  
  theme(legend.position = "none") 
ggsave("rob_mscw.png", width = 16, height = 8, dpi = 300)



##########################
##Model Assumption Checks
##########################

omit_bias <- read.csv("omit_bias.csv")
#order: past aid neighbour, past aid own, past insec, past insec neighbour

d1 <- density(omit_bias$a2_base[1:10])
d2 <- density(omit_bias$a2_base[1:10])
d3 <- density(omit_bias$a5_base[1:10])

png("dens_plot.png", width = , height = 600)  
par(mfrow = c(4, 3))  

#past aid neighbour
plot(d1, col = "#005555", lwd = 2, main = expression(hat(alpha)[1]),
     xlab = "", ylab = "Density")
abline(v = omit_bias$a1_base[11], col = "black", lwd = 2, lty = 2)

plot(d2, col = "#005555", lwd = 2, main = expression(hat(alpha)[2]),
     xlab = "", ylab = "")
abline(v = omit_bias$a2_base[11], col = "black", lwd = 2, lty = 2)  
mtext("Neighbours' Past Food Aid", side = 1, line = 4, cex = 0.8)

plot(d3, col = "#005555", lwd = 2, main = expression(hat(alpha)[5]),
     xlab = "", ylab = "")
abline(v = omit_bias$a5_base[11], col = "black", lwd = 2, lty = 2)  

#past aid own
plot(d1, col = "#005555", lwd = 2, main = expression(hat(alpha)[1]),
     xlab = "", ylab = "Density")
abline(v = omit_bias$a1_base[12], col = "black", lwd = 2, lty = 2)

plot(d2, col = "#005555", lwd = 2, main = expression(hat(alpha)[2]),
     xlab = "", ylab = "")
abline(v = omit_bias$a2_base[12], col = "black", lwd = 2, lty = 2)  
mtext("Own Past Food Aid", side = 1, line = 4, cex = 0.8)  

plot(d3, col = "#005555", lwd = 2, main = expression(hat(alpha)[5]),
     xlab = "", ylab = "")
abline(v = omit_bias$a5_base[12], col = "black", lwd = 2, lty = 2)  

#past insec
plot(d1, col = "#005555", lwd = 2, main = expression(hat(alpha)[1]),
     xlab = "", ylab = "Density")
abline(v = omit_bias$a1_base[13], col = "black", lwd = 2, lty = 2)

plot(d2, col = "#005555", lwd = 2, main = expression(hat(alpha)[2]),
     xlab = "", ylab = "")
abline(v = omit_bias$a2_base[13], col = "black", lwd = 2, lty = 2)  
mtext(" Own Past Food Insecurity", side = 1, line = 4, cex = 0.8)  

plot(d3, col = "#005555", lwd = 2, main = expression(hat(alpha)[5]),
     xlab = "", ylab = "")
abline(v = omit_bias$a5_base[13], col = "black", lwd = 2, lty = 2)  

#past insec neighbour
plot(d1, col = "#005555", lwd = 2, main = expression(hat(alpha)[1]),
     xlab = "", ylab = "Density")
abline(v = omit_bias$a1_base[14], col = "black", lwd = 2, lty = 2)

plot(d2, col = "#005555", lwd = 2, main = expression(hat(alpha)[2]),
     xlab = "", ylab = "")
abline(v = omit_bias$a2_base[14], col = "black", lwd = 2, lty = 2)  
mtext(" Neighbours' Past Food Insecurity", side = 1, line = 4, cex = 0.8)  

plot(d3, col = "#005555", lwd = 2, main = expression(hat(alpha)[5]),
     xlab = "", ylab = "")
abline(v = omit_bias$a5_base[14], col = "black", lwd = 2, lty = 2)  

dev.off()



###########################
##Treatment-Response Curves
###########################

treat_res_df <- read.csv("treat_res_curves.csv")

#Get Aid SD and 2019 value
food_df <- read.csv("../data/food_df_ana.csv")

food_df$sd_aid <- sd(food_df$ad_sdg2_aid, na.rm = TRUE)

aid_sd <- food_df %>%
  group_by(country) %>%
  summarise(sd_aid = sd(ad_sdg2_aid, na.rm = TRUE))
food_df <- food_df %>% filter(year == 2019) %>% select(country, ad_sdg2_aid, sd_aid)
food_df$sd_aid <- sdaid_sd$sd_aid

treat_res_df <- merge(treat_res_df, food_df, by.x = "country_name", by.y = "country")

treat_res_df <- treat_res_df %>%
  group_by(country, t_value, country_name, ad_sdg2_aid, sd_aid) %>%
  summarise(mean_value = mean(value), 
            sd_value = sd(value))

unique_countries <- unique(treat_res_df$country)
country_groups <- split(unique_countries, (seq_along(unique_countries) - 1) %/% 12)
custom_palette <- c("#005555", "#550055", "#000808", "#555500", "#552b00", "#002b55",  "#550000", "#005500", "#00a2a2", "#55002b","#002222",  "#00552b")

counter <- 0
for (country_group in country_groups) {
  chunk <- treat_res_df %>% filter(country %in% country_group)
  chunk_small <- chunk %>%
    filter(t_value >= (ad_sdg2_aid - sd_aid) & t_value <= (ad_sdg2_aid + sd_aid))
  
  chunk_plot <- ggplot(chunk_small, aes(x = t_value, y = mean_value, color = country_name)) +
    geom_line() +  
    geom_ribbon(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value, fill = country_name), 
                alpha = 0.2) +  
    labs(title = "Treatment Response Curves",
         x = "Food Aid, Million USD",
         y = "Relative Reduction in Food Insecurity",
         color = "Country",  
         fill = "Country") +
    scale_x_continuous(limits = c(0, 600)) +  
    scale_color_manual(values = custom_palette) + 
    scale_fill_manual(values = custom_palette) +
    theme_minimal() +
    theme_bw() +
    facet_wrap(~ country_name, nrow = 4, ncol = 3) +  
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = ad_sdg2_aid, color = country_name), linetype = "dashed", size = 1)
  
  name <- paste("chunk_plot_", as.character(counter), ".png")
  counter <- counter+1
  ggsave(name, chunk_plot, width = 16, height = 8, dpi = 300)
  
}

##Plot representative values

food_df$country[food_df$ad_sdg2_aid == min(food_df$ad_sdg2_aid)]
food_df$country[food_df$ad_sdg2_aid == max(food_df$ad_sdg2_aid)]
food_df$country[food_df$ad_sdg2_aid == median(food_df$ad_sdg2_aid)]


#-> Jamaica, Kenya, Georgia

highligh_treatres <- treat_res_df %>% filter(country_name == "Jamaica" | country_name == "Kenya" | country_name == "Georgia")
highligh_treatres_plot <- ggplot(highligh_treatres, aes(x = t_value, y = mean_value, color = country_name)) +
  geom_line() +  
  geom_ribbon(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value, fill = country_name), 
              alpha = 0.2) +  
  labs(title = "Treatment Response Curves",
       x = "Food Aid, Million USD",
       y = "Relative Reduction in Food Insecurity",
       color = "Country",  
       fill = "Country") +
  scale_x_continuous(limits = c(0, 600)) +  
  scale_color_manual(values = custom_palette) + 
  scale_fill_manual(values = custom_palette) +
  theme_minimal() +
  theme_bw() +
  facet_wrap(~ country_name, nrow = 4, ncol = 3) +  
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = ad_sdg2_aid, color = country_name), linetype = "dashed", size = 1)

ggsave("highlight_treatres_curves.png", highligh_treatres_plot, width = 16, height = 8, dpi = 300)
