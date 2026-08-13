#####
# This script houses functions to assess the output of an spOccupancy model

library(tidyverse)
library(tictoc)

#####
# Assess linear combination convergence 
#####

# compute the linear combination w*_i(s_j) for one species, one site
get_lin_comb <- function(data, 
                         species = 1, 
                         site = 1){
  if(!is.numeric(species)){
    species <- which(data$sp.names == species)
  }
  
  num_chains <- data$n.chains
  num_samples <- (data$n.samples - data$n.burn) / data$n.thin
  
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
                          random = FALSE, 
                          species = 1, 
                          site = 1){
  if(!is.numeric(species)){
    species <- which(data$sp.names == species)
  }
  
  num_chains <- data$n.chains
  num_samples <- (data$n.samples - data$n.burn) / data$n.thin
  
  if(random == TRUE){
    species <- sample(1:num_species, 1) 
    site <- sample(1:num_sites, 1)
  }
  
  temp_df <- get_lin_comb(data = data, 
                          species = species, 
                          site = site)
  
  temp_df |> 
    mutate(iteration = 1:num_samples) |> 
    pivot_longer(cols = -c(iteration), names_to = "chain", values_to = "value") |> 
    ggplot(aes(x = iteration, y = value, color = chain)) + 
    geom_line(alpha = 0.8) + 
    geom_smooth(fill = "black", method = 'gam', formula = y ~ s(x, bs = "cs")) +
    labs(title = paste0("Site ", site, " species ", species, " (", data$sp.names[species], ") Linear combination trace plot")) + 
    theme_bw()
}



#####
# Determine which chain has better likelihood, i.e., which mode is better... 
#####

get_elpd <- function(data){
  num_chains <- data$n.chains
  num_samples <- (data$n.samples - data$n.burn) / data$n.thin
  elpds <- numeric(3)
  lower <- seq(1, num_chains * num_samples, by = num_samples)
  upper <- seq(num_samples, num_chains * num_samples, by = num_samples)
  for(i in 1:num_chains){
  elpds[i] <- sum(apply(data$like.samples[lower[i]:upper[i],,], c(2, 3), 
                        function(a) log(mean(a))), na.rm = TRUE)
  }
  return(elpds)
}

elpd_diff <- function(data, best_chain = TRUE, best_elpd = TRUE){
  num_chains <- data$n.chains
  num_samples <- (data$n.samples - data$n.burn) / data$n.thin
  elpd <- get_elpd(data)
  
  temp_diff <- max(elpd) - min(elpd)
  if(best_chain == TRUE & best_elpd == TRUE){
    return(list(difference = temp_diff, 
                best_chain = which.max(elpd), 
                best_elpd  = elpd[which.max(elpd)]))
  } else if(best_elpd == TRUE) {
    return(list(difference = temp_diff, 
                best_elpd  = elpd[which.max(elpd)]))
  } else if(best_chain == TRUE){
    return(list(difference = temp_diff, 
                best_chain = which.max(elpd)))
  } else {
    return(list(difference = temp_diff))
  }
}

get_lhood <- function(data){
  num_chains <- data$n.chains
  num_samples <- (data$n.samples - data$n.burn) / data$n.thin
  lhoods  <- matrix(NA, nrow = num_samples, ncol = num_chains)
  lower <- seq(1, num_chains * num_samples, by = num_samples)
  upper <- seq(num_samples, num_chains * num_samples, by = num_samples)
  for(i in 1:num_chains){
    lhoods[,i] <- apply(data$like.samples[lower[i]:upper[i],,], 1, 
                      function(a) log(mean(a)))
  }
  return(lhoods)
}

plot_lhood <- function(data){
  num_chains <- data$n.chains
  num_samples <- (data$n.samples - data$n.burn) / data$n.thin
  temp <- get_lhood(data)
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


# reset initial values based on chain
reset_inits <- function(data, which_chain = 1){
  num_chains <- data$n.chains
  num_samples <- (data$n.samples - data$n.burn) / data$n.thin
  lower <- seq(1, num_chains * num_samples, by = num_samples)
  upper <- seq(num_samples, num_chains * num_samples, by = num_samples)
  i <- which_chain
  inits <- list(beta.comm = colMeans(out$beta.comm.samples[lower[i]:upper[i],]),
                beta = matrix(colMeans(out$beta.samples[lower[i]:upper[i], ]), 
                              nrow = dim(data_list$y)[1]),
                tau.sq.beta = 
                  colMeans(out$tau.sq.beta.samples[lower[i]:upper[i], ]),
                lambda = 
                  matrix(colMeans(out$lambda.samples[lower[i]:upper[i], ]), 
                                ncol = num_factors),
                phi = colMeans(out$theta.samples[lower[i]:upper[i], ]))
  return(inits)
}

fit_until_converged <- function(data, compute_elpd = TRUE){
  num_chains <- data$n.chains
  num_samples <- (data$n.samples - data$n.burn) / data$n.thin
  tic("Function fit_until_converged")
  if(compute_elpd == TRUE){
    elpd <- elpd_diff(data)
    inits <- reset_inits(data, which_chain = elpd$best_chain)
    best_elpds <- elpd$best_elpd
  } else {
    elpd <- list(difference = 999, best_chain = 1)
    inits <- reset_inits(data, which_chain = elpd$best_chain, 
                         num_chains = 1, 
                         num_samples = 5000)
    best_elpds <- 999
  }
  
  runs <- 2

  while(elpd$difference > 5 & runs < 10){
    rm(out)
    runs <- runs + 1
    
    print("#~#~#~#")
    print(paste0("Model run ", runs, " on chain ", elpd$best_chain))
    print(paste0("     because elpd difference was ", elpd$diff))
    print("#~#~#~#")
    
    tic(msg = paste0("Model run ", runs, " on chain ", elpd$best_chain))
    out <<- sfJSDM(formula = jsdm_formula, 
                  data = data_list, 
                  inits = inits, 
                  n.batch = num_batch, 
                  batch.length = batch_length, 
                  accept.rate = 0.43, 
                  priors = priors, 
                  n.factors = num_factors,
                  cov.model = cov_model, 
                  tuning = tuning, 
                  # n.omp.threads = num_omp_threads, 
                  verbose = TRUE, 
                  NNGP = TRUE, 
                  n.neighbors = num_neighbors, 
                  n.report = num_report, 
                  n.burn = num_burn, 
                  n.thin = num_thin, 
                  n.chains = num_chains)
    toc()
    saveRDS(out, paste0("nps_herbs_northeast_spatialPlus_k300_", num_factors, "factors_modelRun", runs, "_", Sys.Date(), ".rds"))
    elpd <- elpd_diff(out)
    best_elpds <- c(best_elpds, elpd$best_elpd)
    inits <- reset_inits(out, which_chain = elpd$best_chain)
  }
  
  print("#~#~#~#")
  print(paste0("Total runs: ", runs))
  print(best_elpds)
  print("#~#~#~#")
  toc()
  return(out)
}


