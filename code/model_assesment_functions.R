#####
# This script houses functions to assess the output of an spOccupancy model

library(tidyverse)

#####
# Assess linear combination convergence 
#####

# compute the linear combination w*_i(s_j) for one species, one site
get_lin_comb <- function(data, 
                         species = 1, 
                         site = 1, 
                         num_chains = 3, 
                         num_samples = 3000){
  if(!is.numeric(species)){
    species <- which(data$sp.names == species)
  }
  
  temp_df <- data.frame(chain1 = numeric(num_samples))
  
  for(i in 1:num_chains){
    for(j in 1:num_samples){
      ind <- (i - 1) * num_samples + j
      temp_df[j, i] <- 
        matrix(data$lambda.samples[ind, ], nrow = 36)[species, ] %*% 
        data$w.samples[ind, , site]
    }
  }
  colnames(temp_df) <- paste0("chain", 1:num_chains)
  return(temp_df)
}

# plot samples of linear combination w*_i(s_j) for one species one site
plot_lin_comb <- function(data, 
                          species = 1, 
                          site = 1, 
                          num_chains = 3, 
                          num_samples = 3000){
  if(!is.numeric(species)){
    species <- which(data$sp.names == species)
  }
  
  temp_df <- get_lin_comb(data = data, 
                          species = species, 
                          site = site, 
                          num_chains = num_chains, 
                          num_samples = num_samples)
  
  temp_df |> 
    mutate(iteration = 1:num_samples) |> 
    pivot_longer(cols = -c(iteration), names_to = "chain", values_to = "value") |> 
    ggplot(aes(x = iteration, y = value, color = chain)) + 
    geom_line(alpha = 0.8) + 
    geom_smooth(fill = "black", method = 'gam', formula = y ~ s(x, bs = "cs")) +
    labs(title = paste0("Linear combination trace plot for species ", species, " (", data$sp.names[species], ") at site ", site)) + 
    theme_bw()
}



#####
# Determine which chain has better likelihood, i.e., which mode is better... 
#####

get_elpd <- function(data, num_chains = 3, num_samples = 3000){
  elpds <- numeric(3)
  for(i in 1:num_chains){
  elpds[i] <- sum(apply(data$like.samples[lower[i]:upper[i],,], c(2, 3), 
                        function(a) log(mean(a))), na.rm = TRUE)
  }
  return(elpds)
}

elpd_diff <- function(data, num_chains = 3, num_samples = 3000, 
                      best_chain = TRUE){
  elpd <- get_elpd(data, num_chains = num_chains, num_samples = num_samples)
  temp_diff <- max(elpd) - min(elpd)
  if(best_chain == TRUE){
    return(list(difference = temp_diff, best_chain = which.max(elpd)))
  } else {
    return(list(difference = temp_diff))
  }
}

get_lhood <- function(data, num_chains = 3, num_samples = 3000){
  lhoods  <- matrix(NA, nrow = num_samples, ncol = num_chains)
  lower <- seq(1, num_chains * num_samples, by = num_samples)
  upper <- seq(num_samples, num_chains * num_samples, by = num_samples)
  for(i in 1:num_chains){
    lhoods[,i] <- apply(data$like.samples[lower[i]:upper[i],,], 1, 
                      function(a) log(mean(a)))
  }
  return(lhoods)
}

plot_lhood <- function(data, num_chains = 3, num_samples = 3000){
  temp <- get_lhood(data, num_chains = num_chains, num_samples = num_samples)
  lhoods <- data.frame(temp)
  colnames(lhoods) <- paste0("chain", 1:num_chains)
  lhoods |> 
    mutate(iteration = 1:num_samples) |> 
    pivot_longer(cols = -c(iteration), names_to = "chain", values_to = "lhood") |> 
    ggplot(aes(x = iteration, y = lhood, color = chain)) + 
    geom_line(alpha = 0.8) + 
    geom_smooth(fill = "black", method = 'gam', formula = y ~ s(x, bs = "cs")) +
    labs(title = "Likelihood trace plot") + 
    theme_bw()
}





