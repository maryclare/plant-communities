#####
# This script runs the spOccupancy model 
#     requires: dataset (current is nps_herbs_northeast_spOcc_data.rds)



library(spOccupancy)
library(coda)

# load the plot, taxa, and climate covariates data
data_list <- readRDS("./data/nps_herbs_northeast_spOcc_data.rds")
source("./code/model_assesment_functions.R")

# settings: 
num_factors     <- 3
num_neighbors   <- 5
cov_model       <- "exponential"
num_species     <- nrow(data_list$y)
num_sites       <- nrow(data_list$coords)
batch_length    <- 25 # default - documentation suggests leaving at 25
num_batch       <- 5000 # num_iter = batch_length * num_batch
num_burn        <- 75000
num_thin        <- 10 #
num_chains      <- 1
tuning          <- list(phi = 0.5) # adjusts adaptive tuning for phi
# num_omp_threads <- 4
verbose         <- TRUE
num_report      <- 500 # reports after number of batches

# Model formula
jsdm_formula <- ~ scale(tmin) + I(scale(tmin)^2) + scale(ppt) + I(scale(ppt)^2) 

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
temp <- lm(yhat~scale(tmin) + I(scale(tmin)^2) + scale(ppt) + I(scale(ppt)^2),
           data=temp_df)
betas <- data.frame(temp$coefficients)

for(i in 2:ncol(response)){
  yhat <- temp_df[,i]
  temp <- lm(yhat~scale(tmin) + I(scale(tmin)^2) + scale(ppt) + I(scale(ppt)^2),
             data=temp_df)
  betas <- cbind(betas, temp$coefficients)
}
colnames(betas) <- rownames(data_list$y)
betas <- t(betas)
betas_mean <- data.frame(Intercept = mean(betas[, 1]),
                         tmin = mean(betas[, 2]),
                         tmin.2 = mean(betas[, 3]),
                         ppt = mean(betas[, 4]),
                         ppt.2 = mean(betas[, 5])) |>
  as.matrix.data.frame()
betas_var  <- data.frame(Intercept = var(betas[, 1]),
                         tmin = var(betas[, 2]),
                         tmin.2 = var(betas[, 3]),
                         ppt = var(betas[, 4]),
                         ppt.2 = var(betas[, 5])) |>
  as.matrix.data.frame()

inits      <- list(beta.comm = betas_mean,
                  beta = betas,
                  tau.sq.beta = betas_var,
                  lambda = lambda_inits,
                  phi = 3 / mean(dist_matrix))


#####
# Priors
#####
priors <- list(beta.comm.normal = list(mean = 0, var = 2.72),
               tau.sq.beta.ig = list(a = 0.1, b = 0.1),
               phi.unif = list(3 / max(dist_matrix), 3 / min(dist_matrix)))
               #phi.unif = list(c(3/(2.5E06),3/(5E05),3/(5E04),3/(1E03),3/(100)), c(3/(5E05),3/(5E04),3/(1E03), 3/(100),3/(5))))
               #phi.unif = list(c(3/(100),3/(1E03),3/(5E04),3/(5E05),3/(2.5E06)), c(3/(5),3/(100),3/(1E03),3/(5E04),3/(5E05))))


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
              # n.omp.threads = num_omp_threads, 
              verbose = TRUE, 
              NNGP = TRUE, 
              n.neighbors = num_neighbors, 
              n.report = num_report, 
              n.burn = num_burn, 
              n.thin = num_thin, 
              n.chains = num_chains)

# Second run, multiple chains, and set inits to mean of first run
# settings: 
num_factors     <- 3
num_neighbors   <- 5
cov_model       <- "exponential"
num_species     <- nrow(data_list$y)
batch_length    <- 25 # default - documentation suggests leaving at 25
num_batch       <- 2000 # num_iter = batch_length * num_batch
num_burn        <- 20000
num_thin        <- 10 #
num_chains      <- 3
tuning          <- list(phi = 0.5) # adjusts adaptive tuning for phi
# num_omp_threads <- 4
verbose         <- TRUE
num_report      <- 100 # reports after number of batches

inits <- list(beta.comm = colMeans(out$beta.comm.samples[1:5000,]),
              beta = matrix(colMeans(out$beta.samples[1:5000, ]), 
                            nrow = dim(data_list$y)[1]),
              tau.sq.beta = colMeans(out$tau.sq.beta.samples[1:5000, ]),
              lambda = matrix(colMeans(out$lambda.samples[1:5000, ]), 
                              ncol = num_factors),
              phi = colMeans(out$theta.samples[1:5000, ]))

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
