##### Script to create credible intervals for each covariate for each species

library(spOccupancy)
library(tidyverse)
library(gridExtra)

plot_sfJSDM_covariate_effects <- function(sfJSDM_output, 
                                          species_names = NULL,
                                          n_species,
                                          covariate_names = NULL,
                                          cred_level = 0.93,
                                          point_size = 3,
                                          line_size = 1) {
  
  # Extract posterior samples for occupancy coefficients
  # These are stored in sfJSDM_output$beta.samples
  beta_samples <- sfJSDM_output$beta.samples
  
  # Get dimensions: samples, species, covariates
  n_samples <- dim(beta_samples)[1]
  n_covariates <- dim(beta_samples)[2] / n_species
  
  # Get covariate names if available
  if (is.null(covariate_names)) {
    covariate_names <- colnames(beta_samples)[(1:n_covariates - 1) * n_species + 1]
    }
  if (is.null(covariate_names)) {
      covariate_names <- paste0("Covariate_", 1:n_covariates)
    }
  
  # Get species names if not provided
  if (is.null(species_names)) {
    species_names <- dimnames(beta_samples)[[2]]
    if (is.null(species_names)) {
      species_names <- paste0("Species_", 1:n_species)
    }
  }
  
  # Calculate credible interval bounds
  lower_tail <- (1 - cred_level) / 2
  upper_tail <- 1 - lower_tail
  
  # Create a list to store plots
  plot_list <- list()
  
  # Loop through each covariate
  for (cov in 1:n_covariates) {
    
    # Extract samples for this covariate across all species
    indices <- ((cov - 1) * n_species + 1):(cov * n_species)
    cov_samples <- beta_samples[, indices]  # dimensions: samples x species
    
    # Calculate credible intervals for each species
    ci_data <- data.frame(
      species = factor(species_names, levels = rev(species_names)),
      mean = apply(cov_samples, 2, mean),
      lower = apply(cov_samples, 2, quantile, probs = lower_tail),
      upper = apply(cov_samples, 2, quantile, probs = upper_tail)
    )
    
    # Check if zero is in the credible interval
    ci_data$significance <- ifelse(
      (ci_data$lower < 0 & ci_data$upper > 0), 
      "Covers 0", 
      "Include"
    )
    
    # Create the plot
    p <- ggplot(ci_data, aes(x = species, y = mean, color = significance)) +
      geom_point(size = point_size) +
      geom_linerange(aes(ymin = lower, ymax = upper), size = line_size) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
      coord_flip() +
      labs(
        title = covariate_names[cov],
        x = "Species",
        y = paste0("Effect Estimate (", cred_level * 100, "% CI)"),
        color = ""
      ) +
      scale_color_manual(values = c("Include" = "#E69F00", 
                                    "Covers 0" = "#56B4E9")) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.x = element_line(color = "gray95"),
        legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    plot_list[[cov]] <- p
  }
  
  # Return list of plots
  return(plot_list)
}

# 
# # After running sfJSDM()
# plots <- plot_sfJSDM_covariate_effects(out_soil_15cm, 
#                                        species_names = rownames(data$y),
#                                        n_species = 98, 
#                                        covariate_names = colnames(data$covs)[c(1:4, 8:11, 13, 20, 27)], 
#                                        cred_level = 0.95)
# 
# # View individual plots
# plots[[2]]  # First covariate
# 
# Save all plots
# for (i in seq_along(plots)) {
#   ggsave(paste0("./data/covariate_", covariate_names[i], ".png"), plots[[i]], width = 10, height = 8)
# }
