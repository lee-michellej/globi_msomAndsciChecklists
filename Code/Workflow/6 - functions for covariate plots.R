#######################################
#######################################
## Author: Dr. Graziella DiRenzo
##
## Date Created: 2025-01-27
##
## Copyright (c) Graziella DiRenzo, 2025
## Email: gdirenzo@umass.edu
#######################################
#######################################


#######################################
## Code objectives:
#######################################


# To write code that compares the output of 6 different Globi models

# The model types & names are:
# 1. No bee/plant specification = no_bee_plant
# 2. bee-specific intercepts = bee_species
# 3. plant-specific intercepts = plant_species
# 4. bee family intercepts = bee_family
# 5. plant family intercepts = plant_family
# 6. bee and plant family intercepts = bee_plant_family



#######################################
## Output of code:
#######################################


# The function code for making:
  # continuous plots
  # categorical variable plots


#######################################
############ Table of Contents ########
#######################################


# 1. Write a function to plot the relationship between response and continous covariate
  # response_continous_cov_plot()
# 2. Write a function to plot the relationship between response and categorical covariate(s) 
  # response_factor_cov_plot()

#######################################
#######################################
#######################################





# 1. Write a function to plot the relationship between response and continous covariate ------------------------------------




# Write function for response vs continous covariate plot
response_continous_cov_plot <- function(out_df, 
                                        beta,      # For all it will be = out_df$beta_psi.2.
                                        intercept, # For most it will be: out_df$beta_psi.1.
                                        mod_name,
                                        x_lab_text,
                                        y_lab_text){
  
  # Create empty data frame
  stats_df <- data.frame(mod_name = rep(mod_name, times = 2),
                         beta_direction = c("greater than 0", "less than 0"),
                         proportion = c(NA, NA))
  
  
  # Calculate the statistics:
  # What proportion of the mass is > 0?
  stats_df[1, 3] <- mean(beta > 0)
  
  # What proportion of the mass is < 0?
  stats_df[2, 3] <- mean(beta < 0)
  
  
  # Create a dataframe with the parameter estimates from each MCMC iteraction
  param_df <- data.frame(beta_psi = beta, 
                         intercept = intercept,
                         iteration = 1:length(intercept))
  
  # Add a column with the covariate
  pred_df <- expand_grid(param_df, 
                         tibble(Cov.scaled = seq(-3, 3, length.out = 50)))
  
  
  # Add predictions
  pred_df$pred <- plogis(pred_df$intercept + 
                           pred_df$beta_psi * pred_df$Cov.scaled)
  
  # Round to ensure grouping works properly
  pred_df$Cov.scaled <- round(pred_df$Cov.scaled, 2)
  
  # Group by Cov.scaled and calculate quantiles
  size_vals <- unique(pred_df$Cov.scaled)
  
  # Create an empty data frame
  credible_intervals <- data.frame(
    Cov.scaled = size_vals,
    lower_95 = NA,
    upper_95 = NA,
    lower_80 = NA,
    upper_80 = NA,
    lower_50 = NA,
    upper_50 = NA,
    me_pred = NA
  )
  
  # Loop through all calculations for the credible intervals
  for(i in 1:length(size_vals)) {
    
    subset_data <- pred_df$pred[pred_df$Cov.scaled == size_vals[i]]
    
    credible_intervals$lower_95[i] <- quantile(subset_data, 0.025)
    credible_intervals$upper_95[i] <- quantile(subset_data, 0.975)
    credible_intervals$lower_80[i] <- quantile(subset_data, 0.10)
    credible_intervals$upper_80[i] <- quantile(subset_data, 0.90)
    credible_intervals$lower_50[i] <- quantile(subset_data, 0.25)
    credible_intervals$upper_50[i] <- quantile(subset_data, 0.75)
    
    credible_intervals$me_pred[i] <- median(subset_data)
  }
  
  
  
  # Create data frame to label each scenario if it is significant or not
  annotation_df <- data.frame(
    # x-axis value
    bee_size_value = mean(pred_df$Cov.scaled),
    # y-axis value
    prob_value = max(credible_intervals$upper_95),
    # label
    label = paste0("Pr(Slope is greater than 0) = ", round(stats_df[1, 3], dig = 2)))
  
  
  # Create plot
  gplot <- ggplot(data = credible_intervals) +
    # 95% CI - lightest
    geom_ribbon(aes(x = Cov.scaled, ymin = lower_95, ymax = upper_95), 
                alpha = 0.2, fill = "darkblue") +
    # 80% CI - medium
    geom_ribbon(aes(x = Cov.scaled, ymin = lower_80, ymax = upper_80), 
                alpha = 0.3, fill = "darkblue") +
    # 50% CI - darkest
    geom_ribbon(aes(x = Cov.scaled, ymin = lower_50, ymax = upper_50), 
                alpha = 0.4, fill = "darkblue") +
    # Mean line
    geom_line(aes(x = Cov.scaled, y = me_pred), 
              col = "darkblue", linewidth = 1.5) +
    ylab(y_lab_text)+
    xlab(x_lab_text)+
    theme(legend.position = "none",
          strip.background = element_rect(colour = "black", fill = "white"),
          strip.text = element_text(size = title.size), 
          panel.background = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = text.size),
          axis.text.y = element_text(size = text.size),
          axis.title.x = element_text(size = title.size),
          axis.title.y = element_text(size = title.size))+
    ggtitle(mod_name)+
    
    # Add annotation if the relationship is significant or not
    geom_text(data = annotation_df, aes(x = bee_size_value, 
                                        y = prob_value,
                                        label = label), 
              size = 3,
              hjust = 0.5, 
              vjust = 0)+
    ## ── give the text room to breathe ───────────────────────────
    expand_limits(y = max(annotation_df$prob_value) * 1.05)
  
  
  return(list(gplot = gplot,
              stats_df = stats_df))
  
}



# 2. Write a function to plot the relationship between response and categorical covariate(s) ----------------------------------------

# Write function for response vs continuous covariate plot
response_factor_cov_plot <- function(out_df, 
                                     num_cov, # either 2 or 4
                                     beta1,  # This is the intercept - always
                                     beta2,
                                     beta3,
                                     beta4,
                                     beta1_name,
                                     beta2_name,
                                     beta3_name,
                                     beta4_name,
                                     mod_name,
                                     x_lab_text,
                                     y_lab_text,
                                     pal_cols){
  
  # Calculating the stats:
  # Setting up 2 conditions:
  # Number of covariates == 1 or 
  # Number of covariates == 4
  if(num_cov == 2){
    
    # Create empty data frame
    stats_df <- data.frame(mod_name = rep(mod_name, times = 1),
                           beta_direction = c(paste0(beta1_name, " greater than ", beta2_name)),
                           proportion = c(NA))
    
    
    # What proportion of the mass is beta1 < (beta1 + beta2)?
    stats_df[1, 3] <- mean(beta1 < (beta1 + beta2))
    
  }
  
  if(num_cov == 4){
    
    # Create empty data frame
    stats_df <- data.frame(mod_name = rep(mod_name, times = 3),
                           beta_direction = c(paste0(beta1_name, " greater than ", beta2_name),
                                              paste0(beta1_name, " greater than ", beta3_name),
                                              paste0(beta1_name, " greater than ", beta4_name)),
                           proportion = c(NA, NA, NA))
    
    
    # What proportion of the mass is beta1 < (beta1 + beta2)?
    stats_df[1, 3] <- mean(beta1 < (beta1 + beta2))
    
    # What proportion of the mass is beta1 < (beta1 + beta3)?
    stats_df[2, 3] <- mean(beta1 < (beta1 + beta3))
    
    # What proportion of the mass is beta1 < (beta1 + beta4)?
    stats_df[3, 3] <- mean(beta1 < (beta1 + beta4))
    
  }
  
  
  # Create dataframes for plotting
  if(num_cov == 2){
    
    # Create a dataframe with the parameter estimates from each MCMC iteraction
    # These are on the logit scale
    pred_df <- data.frame(beta1 = beta1, 
                          beta2 = beta2,
                          iteration = 1:length(beta1))
    
    # Add a column with the covariate
    # These are on the probability scale
    pred_df$beta1_prob <- plogis(pred_df$beta1) 
    pred_df$beta2_prob <- plogis(pred_df$beta1 + pred_df$beta2) 
    
    # Convert to long format
    pred_df_long <- melt(pred_df[,4:5])
    
    # Change column names
    colnames(pred_df_long) <- c("covariate", "probability")
    
    # Change labels
    pred_df_long$covariate <- ifelse(pred_df_long$covariate == "beta1_prob",
                                     beta1_name,
                                     beta2_name)
    
    # Calculate group means
    mu <- ddply(pred_df_long, 
                "covariate",
                summarise, 
                grp.mean = mean(probability))
    
    
    # Compute x-axis range dynamically
    x_min <- min(pred_df_long$probability, na.rm = TRUE)
    x_max <- max(pred_df_long$probability, na.rm = TRUE)
    x_range <- x_max - x_min
    
    # Dynamic offset values (scaling relative to the x-range)
    offset_major <- x_range * 0.03  # Used for text placement
    offset_major1 <- x_range * 0.09  # Used for text placement
    offset_major2 <- x_range * 0.15  # Used for text placement
    offset_minor <- x_range * 0.02  # Used for segment adjustments
    
    # Annotation data frame - this adds the Bayesian p-values to the plot
    annotation_df <- data.frame(
      start =  c(beta1_name),
      end = c(beta2_name),
      probability = c(max(pred_df$beta2_prob) + offset_major1),
      label = c(mean( pred_df$beta1_prob > pred_df$beta2_prob)),
      y_start = 1,
      y_end = 2
    )
    
    # Add a little more room for the labels
    annotation_df <- annotation_df %>% 
      arrange(probability) %>% 
      mutate(
        gap     = 0.08 * x_range,
        label_x = probability + seq_along(probability) * gap,
        bracket_x = label_x - 0.02 * x_range
      )
    
    # Create the plot
    gplot <- ggplot(pred_df_long, aes(x = probability, y = covariate)) +
      stat_dist_gradientinterval(aes(fill = covariate), fill_type = "gradient") +
      scale_fill_manual(values = pal_cols)+
      scale_color_manual(values = pal_cols)+
      xlab(x_lab_text)+
      ylab(y_lab_text)+
      theme_bw()+ 
      theme(legend.position = "none",
            strip.background = element_rect(colour = "black", fill = "white"),
            strip.text = element_text(size = title.size, color = "black"), 
            panel.background = element_rect(colour = "black", fill = NA),
            axis.text.x = element_text(size = text.size, color = "black"),
            axis.text.y = element_text(size = text.size, color = "black"),
            axis.title.x = element_text(size = title.size, color = "black"),
            axis.title.y = element_text(size = title.size, color = "black"))+
      ggtitle(mod_name)+
      ## ── formatting ────────────────────────────────────────────────
      # Remove any ticks & labels > 1.0
      scale_x_continuous(
        breaks = function(lims) {                 # lims = current axis limits
          br <- scales::pretty_breaks()(lims)     # default pretty breaks
          br[br < 1]                             # keep those ≤ 1
        }
      ) + 
      theme(legend.position = "none",
            strip.background = element_rect(colour = "black", fill = "white"),
            strip.text = element_text(size = title.size), 
            panel.background = element_rect(colour = "black", fill = NA),
            axis.text.x = element_text(size = text.size),
            axis.text.y = element_text(size = text.size),
            axis.title.x = element_text(size = title.size),
            axis.title.y = element_text(size = title.size)) +
      ## ── brackets ────────────────────────────────────────────────
      geom_segment(                                         # vertical part
        data = annotation_df,
        aes(x = bracket_x, xend = bracket_x,
            y = y_start,  yend = y_end),
        linewidth = 0.5
      ) +
      geom_segment(                                         # top hook
        data = annotation_df,
        aes(x = bracket_x,                xend = bracket_x - 0.02 * x_range,
            y = y_start,                  yend = y_start),
        linewidth = 0.5
      ) +
      geom_segment(                                         # bottom hook
        data = annotation_df,
        aes(x = bracket_x,                xend = bracket_x - 0.02 * x_range,
            y = y_end,                    yend = y_end),
        linewidth = 0.5
      ) +
      
      ## ── numbers ─────────────────────────────────────────────────
      geom_text(
        data = annotation_df,
        aes(x = label_x,
            y = (y_start + y_end) / 2,
            label = sprintf("%.2f", label)),
        hjust = 0, vjust = 0, size = 3
      ) +
      ## ── give the text room to breathe ───────────────────────────
      expand_limits(x = max(annotation_df$label_x) * 1.05)
    
  }
  
  if(num_cov == 4){
    
    # Create a dataframe with the parameter estimates from each MCMC iteraction
    # These are on the logit scale
    pred_df <- data.frame(beta1 = beta1, 
                          beta2 = beta2,
                          beta3 = beta3,
                          beta4 = beta4,
                          iteration = 1:length(beta1))
    
    # Add a column with the covariate
    # These are on the probability scale
    pred_df$beta1_prob <- plogis(pred_df$beta1) 
    pred_df$beta2_prob <- plogis(pred_df$beta1 + pred_df$beta2) 
    pred_df$beta3_prob <- plogis(pred_df$beta1 + pred_df$beta3) 
    pred_df$beta4_prob <- plogis(pred_df$beta1 + pred_df$beta4) 
    
    # Convert to long format
    pred_df_long <- melt(pred_df[,6:9])
    
    # Change column names
    colnames(pred_df_long) <- c("covariate", "probability")
    
    # Change labels
    pred_df_long$covariate <- ifelse(pred_df_long$covariate == "beta1_prob",
                                     beta1_name,
                                     ifelse(pred_df_long$covariate == "beta2_prob",
                                            beta2_name,
                                            ifelse(pred_df_long$covariate == "beta3_prob",
                                                   beta3_name,
                                                   ifelse(pred_df_long$covariate == "beta4_prob",
                                                          beta4_name, NA))))
    
    # Make pred_df_long$covariate into a factor
    pred_df_long$covariate <- factor(pred_df_long$covariate, levels = c(beta1_name,
                                                                        beta2_name,
                                                                        beta3_name,
                                                                        beta4_name))
    
    # Calculate group means
    mu <- ddply(pred_df_long, 
                "covariate",
                summarise, 
                grp.mean = mean(probability))
    
    
    # Compute x-axis range dynamically
    x_min <- min(pred_df_long$probability, na.rm = TRUE)
    x_max <- max(pred_df_long$probability, na.rm = TRUE)
    x_range <- x_max - x_min
    
    # Dynamic offset values (scaling relative to the x-range)
    offset_major <- x_range * 0.03  # Used for text placement
    offset_major1 <- x_range * 0.09  # Used for text placement
    offset_major2 <- x_range * 0.15  # Used for text placement
    offset_major3 <- x_range * 0.21  # Used for text placement
    offset_minor <- x_range * 0.02  # Used for segment adjustments
    
    # Create annotation data frame with scaled offsets
    annotation_df <- data.frame(
      start =  c(beta1_name, beta1_name, beta1_name),
      end = c(beta2_name, beta3_name, beta4_name),
      probability = c(max(c(pred_df$beta1_prob, pred_df$beta2_prob, 
                            pred_df$beta3_prob, pred_df$beta4_prob))+offset_major1,
                      max(c(pred_df$beta1_prob, pred_df$beta2_prob, 
                            pred_df$beta3_prob, pred_df$beta4_prob))+offset_major2,
                      max(c(pred_df$beta1_prob, pred_df$beta2_prob, 
                            pred_df$beta3_prob, pred_df$beta4_prob))+offset_major3),
      label = c(mean( pred_df$beta1_prob > pred_df$beta2_prob),
                mean( pred_df$beta1_prob > pred_df$beta3_prob),
                mean( pred_df$beta1_prob > pred_df$beta4_prob))
    )
    
    # Add alittle more room for the labels
    annotation_df <- annotation_df %>% 
      arrange(probability) %>% 
      mutate(
        gap     = 0.08 * x_range,
        label_x = probability + seq_along(probability) * gap,
        bracket_x = label_x - 0.02 * x_range
      )
    
    # Create a mapping of color to numeric positions
    color_levels <- c(beta1_name, beta2_name, beta3_name, beta4_name)
    color_map <- setNames(seq_along(color_levels), color_levels)
    
    # Adjust annotation_df to use numeric positions
    annotation_df$y_start <- color_map[annotation_df$start]
    annotation_df$y_end   <- color_map[annotation_df$end]
    
    
    # Create the plot with scaled offsets
    gplot <- ggplot(pred_df_long, aes(x = probability, y = covariate)) +
      stat_dist_gradientinterval(aes(fill = covariate), fill_type = "gradient") +
      scale_fill_manual(values = pal_cols) +
      scale_color_manual(values = pal_cols) +
      xlab(x_lab_text) +
      ylab(y_lab_text) +
      ## ── formatting ────────────────────────────────────────────────
      # Remove any ticks & labels > 1.0
      scale_x_continuous(
        breaks = function(lims) {                 # lims = current axis limits
          br <- scales::pretty_breaks()(lims)     # default pretty breaks
          br[br < 1]                             # keep those ≤ 1
        }
      ) + 
      theme(legend.position = "none",
            strip.background = element_rect(colour = "black", fill = "white"),
            strip.text = element_text(size = title.size), 
            panel.background = element_rect(colour = "black", fill = NA),
            axis.text.x = element_text(size = text.size),
            axis.text.y = element_text(size = text.size),
            axis.title.x = element_text(size = title.size),
            axis.title.y = element_text(size = title.size)) +
      ## ── brackets ────────────────────────────────────────────────
      geom_segment(                                         # vertical part
        data = annotation_df,
        aes(x = bracket_x, xend = bracket_x,
            y = y_start,  yend = y_end),
        linewidth = 0.5
      ) +
      geom_segment(                                         # top hook
        data = annotation_df,
        aes(x = bracket_x,                xend = bracket_x - 0.02 * x_range,
            y = y_start,                  yend = y_start),
        linewidth = 0.5
      ) +
      geom_segment(                                         # bottom hook
        data = annotation_df,
        aes(x = bracket_x,                xend = bracket_x - 0.02 * x_range,
            y = y_end,                    yend = y_end),
        linewidth = 0.5
      ) +
      
      ## ── numbers ─────────────────────────────────────────────────
      geom_text(
        data = annotation_df,
        aes(x = label_x,
            y = (y_start + y_end) / 2,
            label = sprintf("%.2f", label)),
        hjust = 0, vjust = 0, size = 3
      ) +
      
      ## ── give the text room to breathe ───────────────────────────
      expand_limits(x = max(annotation_df$label_x) * 1.05) +  # 5 % extra padding
      ggtitle(mod_name)
    
  }
  
  
  return(list(stats_df = stats_df,
              gplot = gplot))
}




# End script