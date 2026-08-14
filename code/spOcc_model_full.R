#####
# This script runs the spOccupancy model 
#####
# This script runs the full NPS dataset with quadratic covariates including 
#     soil covariates at 15cm depth for `num_factors` factors. 


library(mgcv)        # for Spatial+ confounding adjustment
library(spOccupancy) # for occupancy model
library(coda)        # for trace plots
library(tictoc)      # for timing model runs

# load the plot, taxa, and climate covariates data
data_list <- readRDS("~/plant_communities/data/nps_allfull_wsoil_spOcc_data.rds")
source("~/plant_communities/code/model_assesment_functions.R")

# settings: 
seed_val        <- 8274
num_factors     <- 20
num_neighbors   <- 15
cov_model       <- "exponential"
num_species     <- nrow(data_list$y)
num_sites       <- nrow(data_list$coords)
batch_length    <- 25 # default - documentation suggests leaving at 25
num_batch       <- 3000 # num_iter = batch_length * num_batch
num_burn        <- 50000
num_thin        <- 10 #
num_chains      <- 1
tuning          <- list(phi = 0.5) # adjusts adaptive tuning for phi
num_omp_threads <- 16
verbose         <- TRUE
num_report      <- 100 # reports after number of batches

set.seed(seed_val)
# Model formula
jsdm_formula <- ~ scale(tmax) + scale(tmin) + scale(soil) + 
  scale(ppt) + scale(vpd) + scale(elv) + scale(slp) + 
  scale(asp) + scale(n_tot_15m) + scale(pH_15m) + scale(sand_15m) + 
  I(scale(tmax)^2) + I(scale(tmin)^2) + I(scale(soil)^2) + 
  I(scale(ppt)^2) + I(scale(vpd)^2) + I(scale(elv)^2) + 
  I(scale(slp)^2) + I(scale(asp)^2) + I(scale(n_tot_15m)^2) + 
  I(scale(pH_15m)^2) + I(scale(sand_15m)^2) 


#####
# Initial values
#####
# factor loadings matrix
lambda_inits <- matrix(0, num_species, num_factors)
diag(lambda_inits) <- 1
lambda_inits[lower.tri(lambda_inits)] <- rnorm(sum(lower.tri(lambda_inits)))

# distance matrix between sites
dist_matrix <- dist(data_list$coords)

# inits for betas, and common means and vars: beta.comm and tau.sq.beta
# temp_inits <- readRDS("~/plant_communities/data/inits_nps_allfull_wsoil_spOcc_15cmQuadRun.rds")
# 
# inits      <- list(beta.comm = temp_inits$beta.comm,
#                    beta = temp_inits$beta,
#                    tau.sq.beta = temp_inits$tau.sq.beta,
#                    lambda = lambda_inits,
#                    phi = runif(num_factors, 3 / max(dist_matrix), 3 / min(dist_matrix)))


#####
# Priors
#####
priors <- list(beta.comm.normal = list(mean = 0, var = 2.72),
               tau.sq.beta.ig = list(a = 0.1, b = 0.1),
               phi.unif = list(3 / max(dist_matrix), 3 / min(dist_matrix)))
rm(dist_matrix)


#####
# run the model
#####
# out <- sfJSDM(formula = jsdm_formula, 
#               data = data_list, 
#               inits = inits, 
#               n.batch = num_batch, 
#               batch.length = batch_length, 
#               accept.rate = 0.43, 
#               priors = priors, 
#               n.factors = num_factors,
#               cov.model = cov_model, 
#               tuning = tuning, 
#               n.omp.threads = num_omp_threads, 
#               verbose = TRUE, 
#               NNGP = TRUE, 
#               n.neighbors = num_neighbors, 
#               n.report = num_report, 
#               n.burn = num_burn, 
#               n.thin = num_thin, 
#               n.chains = num_chains)
# 
# saveRDS(out, paste0("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_", num_factors, "factors_initial_", Sys.Date(), ".rds"))
out <- readRDS("~/../../work/pi_twixson_umass_edu/inits_data_nps_full_allCovs_15cmQuad_20factors_initial_2026-06-23.rds")
inits_means <- out$means
inits_sds <- out$sds

# Second run, multiple chains, and set inits to mean of first run
lower <- 1
upper <- (num_batch * batch_length - num_burn) / num_thin

inits <- 
  list(beta.comm = rnorm(length(inits_means$beta.comm), 
                         inits_means$beta.comm, 
                         inits_sds$beta.comm),
       beta = matrix(rnorm(length(inits_means$beta), 
                           inits_means$beta, 
                           inits_sds$beta), 
                     nrow = dim(data_list$y)[1]), 
       tau.sq.beta = rnorm(length(inits_means$tau.sq.beta), 
                           inits_means$tau.sq.beta, 
                           inits_sds$tau.sq.beta), 
       lambda = matrix(rnorm(length(inits_means$lambda), 
                             inits_means$lambda, 
                             inits_sds$lambda), 
                       nrow = dim(data_list$y)[1]), 
       phi = rnorm(length(inits_means$phi),
                   inits_means$phi, 
                   inits_sds$phi))
rm(out)
# settings: 
num_batch       <- 2000 # num_iter = batch_length * num_batch
num_burn        <- 30000
num_chains      <- 1
num_report      <- 200 # reports after number of batches

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

saveRDS(out, paste0("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_", num_factors, "factors_run2_chain2_", Sys.Date(), ".rds"))
