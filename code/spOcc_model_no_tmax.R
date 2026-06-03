#####
# This script runs the spOccupancy model 
#     requires: dataset (current is nps_herbs_northeast_spOcc_data.rds)


library(mgcv)        # for Spatial+ confounding adjustment
library(spOccupancy) # for occupancy model
library(coda)        # for trace plots
library(tictoc)      # for timing model runs

# load the plot, taxa, and climate covariates data
data_list <- readRDS("./data/nps_herbs_northeast_spOcc_data.rds")
source("./code/model_assesment_functions.R")

# settings: 
set.seed        <- 8273
num_factors     <- 3
num_neighbors   <- 5
cov_model       <- "exponential"
num_species     <- nrow(data_list$y)
num_sites       <- nrow(data_list$coords)
batch_length    <- 25 # default - documentation suggests leaving at 25
num_batch       <- 3000 # num_iter = batch_length * num_batch
num_burn        <- 50000
num_thin        <- 10 #
num_chains      <- 1
tuning          <- list(phi = 0.5) # adjusts adaptive tuning for phi
num_omp_threads <- 8
verbose         <- TRUE
num_report      <- 100 # reports after number of batches

#####
# Spatial+ treatment of covariates
#####
data_list$covs$tmaxr <- 
  gam(data_list$covs$tmax ~ 
        s(data_list$coords$X, data_list$coords$Y, k = 100, fx = T), 
      method = "GCV.Cp")$residuals
data_list$covs$tminr <- 
  gam(data_list$covs$tmin ~ 
        s(data_list$coords$X, data_list$coords$Y, k = 100, fx = T), 
      method = "GCV.Cp")$residuals
data_list$covs$soilr <- 
  gam(data_list$covs$soil ~ 
        s(data_list$coords$X, data_list$coords$Y, k = 100, fx = T), 
      method = "GCV.Cp")$residuals
data_list$covs$pptr <- 
  gam(data_list$covs$ppt ~ 
        s(data_list$coords$X, data_list$coords$Y, k = 100, fx = T), 
      method = "GCV.Cp")$residuals
data_list$covs$vpdr <- 
  gam(data_list$covs$vpd ~ 
        s(data_list$coords$X, data_list$coords$Y, k = 100, fx = T), 
      method = "GCV.Cp")$residuals
data_list$covs$elvr <- 
  gam(data_list$covs$elv ~ 
        s(data_list$coords$X, data_list$coords$Y, k = 100, fx = T), 
      method = "GCV.Cp")$residuals


# Model formula
jsdm_formula <- ~ scale(tminr) + scale(soilr) + scale(pptr) + 
  scale(vpdr) + scale(elvr) 
  
  
  # scale(tmaxr) + scale(tminr) + scale(soilr) + scale(pptr) + 
  # scale(vpdr) + scale(elvr) 
  # ~ scale(tminr) + scale(soilr) + scale(pptr) + 
  # scale(vpdr) + scale(elvr) 
    # + I(scale(tminr)^2) + I(scale(pptr)^2) 

# distance matrix between sites
dist_matrix <- dist(data_list$coords)


#####
# Initial values
#####
# factor loadings matrix
lambda_inits <- matrix(0, num_species, num_factors)
diag(lambda_inits) <- 1
lambda_inits[lower.tri(lambda_inits)] <- rnorm(sum(lower.tri(lambda_inits)))


# inits for betas, and common means and vars: beta.comm and tau.sq.beta
response <- data.frame(t(data_list$y))
temp_df  <- cbind(response, data_list$covs)

yhat <- temp_df[,1]
temp <- lm(yhat ~ scale(tminr) + scale(soilr) + scale(pptr) + 
             scale(vpdr) + scale(elvr) ,
           data=temp_df)
betas <- data.frame(temp$coefficients)

for(i in 2:ncol(response)){
  yhat <- temp_df[,i]
  temp <- lm(yhat ~ scale(tminr) + scale(soilr) + scale(pptr) + 
               scale(vpdr) + scale(elvr) ,
             data=temp_df)
  betas <- cbind(betas, temp$coefficients)
}
colnames(betas) <- rownames(data_list$y)
betas <- t(betas)
betas_mean <- data.frame(Intercept = mean(betas[, 1]),
                         #tmaxr = mean(betas[, 2]),
                         tminr = mean(betas[, 2]),
                         soilr = mean(betas[, 3]),
                         pptr = mean(betas[, 4]),
                         vpdr = mean(betas[, 5]),
                         elvr = mean(betas[, 6])) %>% #,
                         #tmin.2 = mean(betas[, 4]),
                         #ppt.2 = mean(betas[, 5])) |>
  as.matrix.data.frame()
betas_var  <- data.frame(Intercept = var(betas[, 1]),
                         #tmaxr = var(betas[, 2]),
                         tminr = var(betas[, 2]),
                         soilr = var(betas[, 3]),
                         pptr = var(betas[, 4]),
                         vpdr = var(betas[, 5]),
                         elvr = var(betas[, 6])) %>% #,
                         #tmin.2 = var(betas[, 4]),
                         #ppt.2 = var(betas[, 5])) |>
  as.matrix.data.frame()

inits      <- list(beta.comm = betas_mean,
                  beta = betas,
                  tau.sq.beta = betas_var,
                  lambda = lambda_inits,
                  phi = runif(num_factors, 3 / max(dist_matrix), 3 / min(dist_matrix)))
                      # c(0.0006, 0.001, 0.01, 0.75, 2))


#####
# Priors
#####
priors <- list(beta.comm.normal = list(mean = 0, var = 2.72),
               tau.sq.beta.ig = list(a = 0.1, b = 0.1),
               # phi.unif = list(c(3 / max(dist_matrix), 0.0005, 0.005, 0.5, 1), 
               #                 c(0.0015, 0.0025, 0.65, 3/ min(dist_matrix), 5)))
               phi.unif = list(3 / max(dist_matrix), 3 / min(dist_matrix)))
               #phi.unif = list(c(3/(2.5E06),3/(5E05),3/(5E04),3/(1E03),3/(100)), c(3/(5E05),3/(5E04),3/(1E03), 3/(100),3/(5))))
               #phi.unif = list(c(3/(100),3/(1E03),3/(5E04),3/(5E05),3/(2.5E06)), c(3/(5),3/(100),3/(1E03),3/(5E04),3/(5E05))))
rm(dist_matrix)

# phi.unif =  list(c(3/(100),3/(1E03),3/(5E04),3/(5E05),3/(2.5E06)), c(3/(5),3/(100),3/(1E03),3/(5E04),3/(5E05)))
# tmp.df <- data.frame(low = phi.unif[[1]], high = phi.unif[[2]])
# inits$phi <- apply(tmp.df, 1, mean)


#####
# run the model
#####
out <- sfJSDM(formula = jsdm_formula, 
              data = data_list, 
              inits = inits, 
              n.batch = num_batch, 
              batch.length = batch_length, 
              accept.rate = 0.43, 
              priors = priors, 
              n.factors = num_factors,
              cov.model = cov_model, 
              tuning = tuning, 
              n.omp.threads = num_omp_threads, 
              verbose = TRUE, 
              NNGP = TRUE, 
              n.neighbors = num_neighbors, 
              n.report = num_report, 
              n.burn = num_burn, 
              n.thin = num_thin, 
              n.chains = num_chains)

saveRDS(out, paste0("nps_herbs_northeast_spatialPlus_k100_", num_factors, "factors_modelRun_initial_", Sys.Date(), ".rds"))

# Second run, multiple chains, and set inits to mean of first run
lower <- 1
upper <- dim(out$lambda.samples)[1]
inits <- list(beta.comm = colMeans(out$beta.comm.samples[lower:upper,]),
              beta = matrix(colMeans(out$beta.samples[lower:upper, ]), 
                            nrow = dim(data_list$y)[1]),
              tau.sq.beta = 
                colMeans(out$tau.sq.beta.samples[lower:upper, ]),
              lambda = 
                matrix(colMeans(out$lambda.samples[lower:upper, ]), 
                       ncol = num_factors),
              phi = colMeans(out$theta.samples[lower:upper, ]))
# settings: 
num_batch       <- 2000 # num_iter = batch_length * num_batch
num_burn        <- 20000
num_chains      <- 3
num_report      <- 100 # reports after number of batches

out <- sfJSDM(formula = jsdm_formula, 
              data = data_list, 
              inits = inits, 
              n.batch = num_batch, 
              batch.length = batch_length, 
              accept.rate = 0.43, 
              priors = priors, 
              n.factors = num_factors,
              cov.model = cov_model, 
              tuning = tuning, 
              n.omp.threads = num_omp_threads, 
              verbose = TRUE, 
              NNGP = TRUE, 
              n.neighbors = num_neighbors, 
              n.report = num_report, 
              n.burn = num_burn, 
              n.thin = num_thin, 
              n.chains = num_chains, 
              k.fold = 10, 
              #k.fold.only = T, 
              k.fold.threads = num_omp_threads)

saveRDS(out, paste0("nps_herbs_northeast_spatialPlus_k100_", num_factors, "factors_modelRun_10foldCV_run2_", Sys.Date(), ".rds"))











# Second run, multiple chains, and set inits to mean of first run
# settings: 
num_batch       <- 2000 # num_iter = batch_length * num_batch
num_burn        <- 20000
num_chains      <- 3
num_report      <- 100 # reports after number of batches


data <- fit_until_converged(out, compute_elpd = TRUE)







# reset inits at mean of post-burnin samples
inits <- reset_inits(out, which_chain = 1, num_chains = 1, num_samples = 5000)

out <- sfJSDM(formula = jsdm_formula, 
              data = data_list, 
              inits = inits, 
              n.batch = num_batch, 
              batch.length = batch_length, 
              accept.rate = 0.43, 
              priors = priors, 
              n.factors = num_factors,
              cov.model = cov_model, 
              tuning = tuning, 
              n.omp.threads = num_omp_threads, 
              verbose = TRUE, 
              NNGP = TRUE, 
              n.neighbors = num_neighbors, 
              n.report = num_report, 
              n.burn = num_burn, 
              n.thin = num_thin, 
              n.chains = num_chains)

plot_lin_comb(out)
plot_lin_comb(out, 
              species = sample(1:num_species, 1), 
              site = sample(1:num_sites, 1))
plot_lin_comb(out, 
              species = sample(1:num_species, 1), 
              site = sample(1:num_sites, 1))
plot_lin_comb(out, 
              species = sample(1:num_species, 1), 
              site = sample(1:num_sites, 1))


# Third run, arbitrarily take the mean of the first chain as the initial values

inits <- list(beta.comm = colMeans(out$beta.comm.samples[1:3000,]),
              beta = matrix(colMeans(out$beta.samples[1:3000, ]), 
                            nrow = dim(data_list$y)[1]),
              tau.sq.beta = colMeans(out$tau.sq.beta.samples[1:3000, ]),
              lambda = matrix(colMeans(out$lambda.samples[1:3000, ]), 
                              ncol = num_factors),
              phi = colMeans(out$theta.samples[1:3000, ]))

out <- sfJSDM(formula = jsdm_formula, 
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

plot_lin_comb(out)
plot_lin_comb(out, 
              species = sample(1:num_species, 1), 
              site = sample(1:num_sites, 1))
plot_lin_comb(out, 
              species = sample(1:num_species, 1), 
              site = sample(1:num_sites, 1))
plot_lin_comb(out, 
              species = sample(1:num_species, 1), 
              site = sample(1:num_sites, 1))

# Fourth run, arbitrarily set inits to mean of third chain
inits <- list(beta.comm = colMeans(out$beta.comm.samples[6001:9000,]),
              beta = matrix(colMeans(out$beta.samples[6001:9000, ]), 
                            nrow = dim(data_list$y)[1]),
              tau.sq.beta = colMeans(out$tau.sq.beta.samples[6001:9000, ]),
              lambda = matrix(colMeans(out$lambda.samples[6001:9000, ]), 
                              ncol = num_factors),
              phi = colMeans(out$theta.samples[6001:9000, ]))

out <- sfJSDM(formula = jsdm_formula, 
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

saveRDS(out, "../nps_herbs_northeast_spOcc_mod_3factors_fourth_run.rds")
